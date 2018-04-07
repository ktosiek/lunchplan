module Main exposing (..)

import Html
import List.Extra as List
import Bootstrap.Navbar as Navbar
import Model exposing (..)
import Types exposing (..)
import PositionForm
import OrderForm
import Sync
import SyncAPI
import View exposing (view)


type alias Order =
    Types.Order


type alias Flags =
    { userID : String, userName : String }


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

        UpdatePositionForm msg ->
            case model.positionForm of
                Just form ->
                    { model
                        | positionForm = Just <| PositionForm.update msg form
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

        UpdateOrderForm msg ->
            { model | orderForm = OrderForm.update msg model.orderForm }
                ! []

        {- TODO Zamawiam! -}
        SaveOrderForm ->
            OrderForm.validator model.orderForm
                |> Result.toMaybe
                |> Maybe.map
                    (\validForm ->
                        Sync.updateOrder model.orderForm.orderId validForm model
                    )
                |> Maybe.map (\( m, c ) -> ( { m | orderForm = OrderForm.newOrder }, c ))
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


updateOrder : Maybe OrderId -> (Order -> Order) -> Model -> Model
updateOrder orderId update_ model =
    let
        update =
            update_ >> fixOrderStatus
    in
        case orderId of
            Just orderId ->
                { model | orders = List.updateIf (\o -> o.id == orderId) update model.orders }

            Nothing ->
                { model | orders = (update (defaultOrder model)) :: model.orders }


defaultOrder : Model -> Order
defaultOrder model =
    let
        maxId =
            model.orders
                |> List.map (.id >> unOrderId)
                |> List.maximum
                |> Maybe.withDefault 0

        newOrderId =
            OrderId (maxId + 1)
    in
        { id = newOrderId
        , place =
            { name = ""
            , link = ""
            , description = ""
            }
        , status = Proposed
        , positions = []
        }


updatePosition : OrderId -> Position -> Model -> Model
updatePosition orderId position =
    updateOrder (Just orderId)
        (\order ->
            case List.findIndex (\p -> p.participant.id == position.participant.id) order.positions of
                Just idx ->
                    { order | positions = List.setAt idx position order.positions }

                Nothing ->
                    { order | positions = position :: order.positions }
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SyncAPI.sync FullSync
        , SyncAPI.order SyncOrder
        , SyncAPI.user SyncParticipant
        , SyncAPI.position SyncPosition
        ]
