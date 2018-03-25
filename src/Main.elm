module Main exposing (..)

import Html exposing (Html, div, program, text)
import Html.Attributes exposing (href)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing


type alias Model =
    { navbar : Navbar.State
    , orders : List Order
    , user : Participant
    }


type OrderStatus
    = Proposed
    | Championed
    | Ordered


allOrderStatuses : List OrderStatus
allOrderStatuses =
    [ Ordered, Championed, Proposed ]


type alias Order =
    { place : Place
    , positions : List Position
    , status : OrderStatus
    }


type alias Position =
    { participant : Participant
    , description : String
    , champion : Bool
    }


type alias Participant =
    { name : String
    , id : ParticipantId
    }


type ParticipantId
    = ParticipantId String


type alias Place =
    { name : String
    , link : String
    , description : String
    }


type Msg
    = NoOp
    | NavbarMsg Navbar.State


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarMsg ) =
            Navbar.initialState NavbarMsg
    in
        ( { navbar = navbarState
          , user = { name = "Piesio Grzesio", id = ParticipantId "pg" }
          , orders =
                [ { place =
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
                , { place =
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
                , { place =
                        { name = "TeleSajgon"
                        , description = "Chińczyk z pudłami"
                        , link = "http://www.telesajgon.pl/ken.html"
                        }
                  , positions =
                        [ { participant = { name = "Piesio Grzesio", id = ParticipantId "pg" }
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
                        , { participant = { name = "Piesio Grzesio", id = ParticipantId "pg" }
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
                , { place =
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
            ++ [ cardsList model [ newOrderCard ]
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


orderCardLane : { a | user : Participant } -> ( OrderStatus, List Order ) -> Html Msg
orderCardLane model ( status, orders ) =
    div []
        [ Html.h3 [] [ text (statusName status) ]
        , orders |> List.map (orderCard model) |> cardsList model
        ]


cardsList : { a | user : Participant } -> List (Card.Config Msg) -> Html Msg
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


newOrderCard : Card.Config Msg
newOrderCard =
    Card.config [ Card.attrs [ Spacing.mb3 ] ]
        |> Card.headerH4 [] [ text "Zaproponuj kolejne miejsce:" ]
        |> Card.block []
            [ Block.text [] [ text "TODO: formularz nowego miejsca :-)" ]
            ]


orderCard : { a | user : Participant } -> Order -> Card.Config Msg
orderCard model order =
    Card.config [ Card.attrs [ Spacing.mb3 ] ]
        |> Card.headerH4 []
            [ text order.place.name ]
        |> Card.block []
            [ Block.text []
                [ Html.a [ href order.place.link ] [ text order.place.link ]
                ]
            , Block.text []
                [ text order.place.description
                ]
            ]
        |> Card.listGroup
            (order.positions
                |> List.map
                    (\p ->
                        ListGroup.li
                            (if p.participant.id == model.user.id then
                                [ ListGroup.primary ]
                             else
                                []
                            )
                            (orderPosition p)
                    )
            )


orderPosition : Position -> List (Html Msg)
orderPosition { participant, description } =
    [ div [] [ text participant.name ]
    , div [ Html.Attributes.align "right" ] [ text description ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        NavbarMsg state ->
            { model | navbar = state } ! []


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
