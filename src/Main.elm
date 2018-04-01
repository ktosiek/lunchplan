module Main exposing (..)

import Html exposing (Html, div, program, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Bootstrap.CDN as CDN
import Bootstrap.Badge as Badge
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Flex as Flex
import Model exposing (..)
import Types exposing (..)
import Utils exposing (..)
import PositionForm
import OrderForm


type alias Order =
    Types.Order


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarMsg ) =
            Navbar.initialState NavbarMsg
    in
        ( { navbar = navbarState
          , user = { name = "Piesio Grzesio", id = ParticipantId "pg" }
          , positionForm = Nothing
          , orderForm = OrderForm.newOrder
          , orders =
                [ { id = OrderId 1
                  , place =
                        { name = "Chicago's Pizza"
                        , description = "Na przeciwko"
                        , link = "TODO"
                        }
                  , positions =
                        [ { participant = { name = "Piesio Grzesio", id = ParticipantId "pg" }
                          , champion = False
                          , description = "Texas"
                          }
                        , { participant = { name = "Kot Psot", id = ParticipantId "kp" }
                          , champion = True
                          , description = "Cztery sery"
                          }
                        ]
                  , status = Championed
                  }
                , { id = OrderId 2
                  , place =
                        { name = "Chicago's Pizza"
                        , description = "Na przeciwko"
                        , link = "TODO"
                        }
                  , positions =
                        [ { participant = { name = "Piesio Grzesio", id = ParticipantId "pg" }
                          , champion = False
                          , description = "Texas"
                          }
                        , { participant = { name = "Kot Psot", id = ParticipantId "kp" }
                          , champion = True
                          , description = "Cztery sery"
                          }
                        ]
                  , status = Ordered
                  }
                , { id = OrderId 3
                  , place =
                        { name = "TeleSajgon"
                        , description = "Chińczyk z pudłami"
                        , link = "http://www.telesajgon.pl/ken.html"
                        }
                  , positions =
                        [ { participant = { name = "j Grzesio", id = ParticipantId "pjg" }
                          , champion = False
                          , description = "Duża zupa MIEN z makaronem sojowym i wołowiną"
                          }
                        , { participant = { name = "Kot Psot", id = ParticipantId "kp" }
                          , champion = False
                          , description = "Banan w cieście"
                          }
                        , { participant = { name = "Piesio Grzesio", id = ParticipantId "pg" }
                          , champion = False
                          , description = "Duża zupa MIEN z makaronem sojowym i wołowiną"
                          }
                        , { participant = { name = "Kot Psot", id = ParticipantId "kp" }
                          , champion = False
                          , description = "Banan w cieście"
                          }
                        , { participant = { name = "l Grzesio", id = ParticipantId "pgl" }
                          , champion = False
                          , description = "Duża zupa MIEN z makaronem sojowym i wołowiną"
                          }
                        , { participant = { name = "Kot Psot", id = ParticipantId "kp" }
                          , champion = False
                          , description = "Banan w cieście"
                          }
                        ]
                  , status = Championed
                  }
                , { id = OrderId 4
                  , place =
                        { name = "Dominos"
                        , description = "nie \"Dominium\""
                        , link = "TODO"
                        }
                  , positions =
                        [ { participant = { name = "Piesio Grzesio", id = ParticipantId "pg" }
                          , champion = True
                          , description = "Duża zupa MIEN z makaronem sojowym i wołowiną"
                          }
                        , { participant = { name = "Kot Psot", id = ParticipantId "kp" }
                          , champion = False
                          , description = "Banan w cieście"
                          }
                        ]
                  , status = Championed
                  }
                ]
          }
        , Cmd.batch
            [ navbarMsg
            ]
        )


view : Model -> Html Msg
view model =
    Html.div [] <|
        [ CDN.stylesheet
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseMedium
            -- Collapse menu at the medium breakpoint
            |> Navbar.primary
            -- Customize coloring
            |> Navbar.brand [ href "#" ] [ text "LunchPlan" ]
            |> Navbar.customItems
                [ Navbar.textItem [] [ text model.user.name ]
                ]
            |> Navbar.view model.navbar
        ]
            ++ (model.orders
                    |> groupOrdersByStatus
                    |> List.map (orderCardLane model)
               )
            ++ [ cardsList model [ newOrderCard model ]
               ]


groupOrdersByStatus : List Order -> List ( OrderStatus, List Order )
groupOrdersByStatus orders =
    let
        groups =
            Dict.groupBy
                (.status >> toString)
                orders
    in
        allOrderStatuses
            |> List.map
                (\s ->
                    ( s
                    , Dict.get (toString s) groups |> Maybe.withDefault []
                    )
                )


statusName : OrderStatus -> String
statusName status =
    case status of
        Ordered ->
            "Zamówione"

        Championed ->
            "Zgłosił się bohater"

        Proposed ->
            "Zaproponowane"


type alias CardContext a =
    { a
        | user : Participant
        , positionForm : Maybe PositionForm.Model
        , orderForm : OrderForm.Model
    }


orderCardLane : CardContext a -> ( OrderStatus, List Order ) -> Html Msg
orderCardLane model ( status, orders ) =
    div []
        [ Html.h3 [] [ text (statusName status) ]
        , orders |> List.map (orderCard model) |> cardsList model
        ]


cardsList : CardContext a -> List (Card.Config Msg) -> Html Msg
cardsList model cards =
    Grid.containerFluid []
        [ cards
            |> List.map
                (\c ->
                    Grid.col [ Col.md4 ] [ Card.view c ]
                )
            |> listDefault (Grid.col [ Col.xs12 ] [ text "Brak zamówień" ])
            |> Grid.row []
        ]


listDefault : a -> List a -> List a
listDefault a l =
    if List.isEmpty l then
        [ a ]
    else
        l


newOrderCard : Model -> Card.Config Msg
newOrderCard model =
    Card.config [ Card.attrs [ Spacing.mb3 ] ]
        |> Card.headerH4 [] [ text "Zaproponuj kolejne miejsce:" ]
        |> (case model.orderForm.orderId of
                -- Inne zamówienie jest już edytowane
                Just orderId ->
                    Card.block []
                        [ Block.text [] [ text "Zakończ edycję przed dodaniem nowego wpisu" ]
                        ]

                -- Edytujemy nowe zamówienie
                Nothing ->
                    OrderForm.view UpdateOrderForm SaveOrderForm model.orderForm
                        |> List.map Block.custom
                        |> Card.block []
           )


orderCard : CardContext a -> Order -> Card.Config Msg
orderCard model order =
    let
        commonListGroupOptions =
            [ ListGroup.attrs [ Flex.block, Flex.justifyBetween, Flex.alignItemsCenter ] ]
    in
        Card.config [ Card.attrs [ Spacing.mb3 ] ]
            |> orderHeaderOrForm order model.orderForm
            |> Card.listGroup
                (order.positions
                    |> List.map
                        (\p ->
                            let
                                isCurrentUser =
                                    p.participant.id == model.user.id

                                options =
                                    commonListGroupOptions |> appendIf isCurrentUser [ ListGroup.primary ]
                            in
                                ListGroup.li options
                                    (orderPositionOrForm model order.id isCurrentUser p)
                        )
                    |> flip List.append
                        [ ListGroup.li commonListGroupOptions
                            (newOrderButtonOrForm model order)
                        ]
                )


orderHeaderOrForm : Order -> OrderForm.Model -> Card.Config Msg -> Card.Config Msg
orderHeaderOrForm order form =
    (if form.orderId == Just order.id then
        Just form
     else
        Nothing
    )
        |> Maybe.map
            (OrderForm.view UpdateOrderForm SaveOrderForm
                >> List.map Block.custom
                >> Card.block []
            )
        |> Maybe.withDefault (orderCardHeader order)


orderCardHeader : Order -> Card.Config Msg -> Card.Config Msg
orderCardHeader order cardConfig =
    cardConfig
        |> Card.headerH4 []
            [ text order.place.name
            , Html.button [ onClick (EditOrder order.id) ] [ text "Zmień" ]
            ]
        |> Card.block []
            [ Block.text []
                [ Html.a [ href order.place.link ] [ text order.place.link ]
                ]
            , Block.text []
                [ text order.place.description
                ]
            ]


newOrderButtonOrForm : CardContext a -> Order -> List (Html Msg)
newOrderButtonOrForm model order =
    if List.member model.user.id <| List.map (.participant >> .id) order.positions then
        []
    else
        model.positionForm
            |> Maybe.filter (\f -> f.orderId == order.id)
            |> Maybe.map (PositionForm.view UpdatePositionForm SavePositionForm)
            |> Maybe.withDefault (newOrderButton order)


newOrderButton : Order -> List (Html Msg)
newOrderButton order =
    [ Html.button [ onClick (OpenPositionForm order.id) ] [ text "Dołącz do zamówienia" ] ]


orderPositionOrForm : CardContext a -> OrderId -> Bool -> Position -> List (Html Msg)
orderPositionOrForm ctx orderId isCurrentUser position =
    ctx.positionForm
        |> Maybe.andThen
            (\form ->
                if form.orderId == orderId && isCurrentUser then
                    Just form
                else
                    Nothing
            )
        |> Maybe.map (PositionForm.view UpdatePositionForm SavePositionForm)
        |> Maybe.withDefault (orderPosition isCurrentUser orderId position)


orderPosition : Bool -> OrderId -> Position -> List (Html Msg)
orderPosition isCurrentUser orderId { participant, description, champion } =
    [ div []
        ([ text participant.name ]
            |> appendIf champion
                [ Badge.pillSuccess [] [ text "☎" ] ]
            |> appendIf isCurrentUser
                [ Html.button
                    [ onClick (OpenPositionForm orderId)
                    , Html.Attributes.align "right"
                    ]
                    [ text "Zmień" ]
                ]
        )
    , div [ Html.Attributes.align "right" ] [ text description ]
    ]


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
            (model.positionForm
                |> Maybe.map
                    (\form ->
                        PositionForm.toPosition model.user form
                            |> Result.map
                                (\position ->
                                    { model | positionForm = Nothing }
                                        |> updatePosition form.orderId position
                                )
                            |> Result.withDefault model
                    )
                |> Maybe.withDefault model
            )
                ! []

        EditOrder orderId ->
            case getOrder orderId model.orders of
                Just order ->
                    { model | orderForm = OrderForm.fromOrder order } ! []

                Nothing ->
                    model ! []

        UpdateOrderForm msg ->
            { model | orderForm = OrderForm.update msg model.orderForm }
                ! []

        SaveOrderForm ->
            (OrderForm.validator model.orderForm
                |> Result.toMaybe
                |> Maybe.map
                    (\validForm ->
                        updateOrder model.orderForm.orderId (OrderForm.toOrder validForm) model
                    )
                |> Maybe.map (\m -> { m | orderForm = OrderForm.newOrder })
                |> Maybe.withDefault model
            )
                ! []


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


fixOrderStatus : Order -> Order
fixOrderStatus order =
    { order
        | status =
            case order.status of
                Ordered ->
                    Ordered

                _ ->
                    if List.any .champion order.positions then
                        Championed
                    else
                        Proposed
    }


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


unOrderId : OrderId -> Int
unOrderId (OrderId i) =
    i


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
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
