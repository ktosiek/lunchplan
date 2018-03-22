module Main exposing (..)

import Html exposing (Html, div, program, text)
import Html.Attributes exposing (href)
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar


type alias Model =
    { navbar : Navbar.State
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
          }
        , Cmd.batch
            [ navbarMsg
            ]
        )


view : Model -> Html Msg
view model =
    Html.div []
        [ CDN.stylesheet
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseMedium
            -- Collapse menu at the medium breakpoint
            |> Navbar.primary
            -- Customize coloring
            |> Navbar.brand [ href "#" ] [ text "LunchPlan" ]
            |> Navbar.view model.navbar
        , Grid.container []
            [ Card.config [ Card.attrs [ Html.Attributes.style [ ( "width", "20rem" ) ] ] ]
                |> Card.block []
                    [ Block.titleH4 [] [ text "Siema!" ]
                    , Block.text [] [ text "Przykładowa karta z kawałkiem tekstu." ]
                    ]
                |> Card.view
            ]
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
