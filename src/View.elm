module View exposing (view, orderCard)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Dict
import Dict.Extra as Dict
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
import Model exposing (Model, Msg(..))
import Types exposing (Order, OrderId, OrderStatus(..), allOrderStatuses, Position, Participant)
import Utils exposing (appendIf)
import PositionForm
import OrderForm


type alias Order =
    Types.Order


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
        , forkMe
        ]
            ++ (model.orders
                    |> groupOrdersByStatus
                    |> List.map (orderCardLane model)
               )
            ++ [ cardsList [ newOrderCard model ]
               ]


forkMe : Html msg
forkMe =
    Html.a [ href "https://github.com/ktosiek/lunchplan" ]
        [ Html.img
            [ Html.Attributes.src "https://s3.amazonaws.com/github/ribbons/forkme_right_green_007200.png"
            , Html.Attributes.style
                [ ( "position", "absolute" )
                , ( "top", "20" )
                , ( "right", "0" )
                , ( "border", "0" )
                ]
            , Html.Attributes.alt "Fork me on GitHub"
            ]
            []
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
        , orders |> List.map (orderCard model) |> cardsList
        ]


cardsList : List (Card.Config Msg) -> Html Msg
cardsList cards =
    Grid.containerFluid []
        [ cards
            |> List.map
                (\c ->
                    Grid.col [ Col.md4 ] [ Card.view c ]
                )
            |> whenEmpty (Grid.col [ Col.xs12 ] [ text "Brak zamówień" ])
            |> Grid.row []
        ]


newOrderCard : Model -> Card.Config Msg
newOrderCard model =
    Card.config [ Card.attrs [ Spacing.mb3 ] ]
        |> Card.headerH4 [] [ text "Zaproponuj kolejne miejsce:" ]
        |> (case model.orderForm.orderId of
                -- Inne zamówienie jest już edytowane
                Just _ ->
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



-- Utils


whenEmpty : a -> List a -> List a
whenEmpty a l =
    if List.isEmpty l then
        [ a ]
    else
        l
