module Example exposing (..)

import Debug
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html
import List.Extra as List
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, class)
import Main exposing (..)
import Fuzzers
import Bootstrap.Card as Card


suite : Test
suite =
    fuzz Fuzzers.order "podświetl użytkownika na karcie" <|
        \order ->
            let
                user =
                    unsafeFirst order.positions |> .participant

                cardContext =
                    { user = user, positionForm = Nothing, orderForm = Nothing }
            in
                orderCard cardContext order
                    |> Card.view
                    |> Query.fromHtml
                    |> Query.find [ class "list-group-item-primary" ]
                    |> Query.contains [ Html.text user.name ]


updateFirst : (a -> a) -> List a -> List a
updateFirst f ls =
    case ls of
        [] ->
            []

        a :: l ->
            (f a) :: l


unsafeFirst : List a -> a
unsafeFirst ls =
    case ls of
        [] ->
            Debug.crash "empty order"

        x :: xs ->
            x
