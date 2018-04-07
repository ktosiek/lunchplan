module Main exposing (main)

import Html
import Maybe.Extra as Maybe
import Bootstrap.Navbar as Navbar
import Model exposing (Model, Msg(..))
import Types
    exposing
        ( Order
        , OrderId(..)
        , OrderStatus(..)
        , isChampioning
        , ParticipantId(..)
        )
import PositionForm
import OrderForm
import Sync
import SyncAPI
import View exposing (view)


type alias Order =
    Types.Order


type alias Flags =
    { userID : String
    , userName : String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( navbarState, navbarMsg ) =
            Navbar.initialState NavbarMsg
    in
        ( { navbar = navbarState
          , user = { name = flags.userName, id = ParticipantId flags.userID }
          , positionForm = Nothing
          , orderForm = OrderForm.newOrder
          , orders = []
          , syncState = Sync.emptyState
          }
        , Cmd.batch
            [ navbarMsg
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        NavbarMsg state ->
            { model | navbar = state } ! []

        OpenPositionForm orderId ->
            case getOrder orderId model.orders of
                Just order ->
                    { model
                        | positionForm = Just <| PositionForm.create order model.user
                    }
                        ! []

                Nothing ->
                    model ! []

        UpdatePositionForm formMsg ->
            case model.positionForm of
                Just form ->
                    { model
                        | positionForm = Just <| PositionForm.update formMsg form
                    }
                        ! []

                Nothing ->
                    model ! []

        SavePositionForm ->
            model.positionForm
                |> Maybe.map PositionForm.validator
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map
                    (\validForm ->
                        Sync.updatePosition model.user validForm model
                    )
                |> Maybe.map (\( m, c ) -> ( { m | positionForm = Nothing }, c ))
                |> Maybe.withDefault (model ! [])

        EditOrder orderId ->
            case getOrder orderId model.orders of
                Just order ->
                    { model | orderForm = OrderForm.fromOrder order } ! []

                Nothing ->
                    model ! []

        UpdateOrderForm formMsg ->
            { model | orderForm = OrderForm.update formMsg model.orderForm }
                ! []

        SaveOrderForm ->
            OrderForm.validator model.orderForm
                |> Result.toMaybe
                |> Maybe.map
                    (\validForm ->
                        Sync.updateOrder model.orderForm.orderId validForm model
                    )
                |> Maybe.map (\( m, c ) -> ( { m | orderForm = OrderForm.newOrder }, c ))
                |> Maybe.withDefault (model ! [])

        OrderOrder orderId ->
            getOrder orderId model.orders
                |> Maybe.filter (isChampioning model.user)
                |> Maybe.map .id
                |> Maybe.map (flip Sync.orderOrder model)
                |> Maybe.withDefault (model ! [])

        FullSync syncData ->
            Sync.applyFullSync syncData model ! []

        SyncParticipant raw ->
            Sync.applyParticipant raw model ! []

        SyncPosition raw ->
            Sync.applyPosition raw model ! []

        SyncOrder rawOrder ->
            Sync.applyOrder rawOrder model ! []


getOrder : OrderId -> List Order -> Maybe Order
getOrder id orders =
    List.filter (\o -> o.id == id) orders
        |> List.head


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ SyncAPI.sync FullSync
        , SyncAPI.order SyncOrder
        , SyncAPI.user SyncParticipant
        , SyncAPI.position SyncPosition
        ]
