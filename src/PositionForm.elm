module PositionForm exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (type_, value, checked)
import Html.Events exposing (onInput, onCheck)
import Verify as V
import String.Verify as VString


type alias ValidForm =
    { orderId : OrderId
    , description : String
    , champion : Bool
    }


create : Model.Order -> Participant -> PositionForm
create order user =
    let
        currentEntry =
            order.positions
                |> List.filter (\p -> p.participant.id == user.id)
                |> List.head
    in
        case currentEntry of
            Just position ->
                { orderId = order.id
                , description = position.description
                , champion = position.champion
                , errors = []
                }

            Nothing ->
                { orderId = order.id
                , description = ""
                , champion = False
                , errors = []
                }


view : PositionForm -> List (Html PositionFormMsg)
view form =
    [ input [ onInput UpdateDescription, value form.description ] []
    , label []
        [ text "Złożę zamówienie"
        , input [ type_ "checkbox", checked form.champion, onCheck UpdateChampion ] []
        ]
    ]
        ++ showErrors form.errors


showErrors : List String -> List (Html PositionFormMsg)
showErrors errors =
    if errors == [] then
        []
    else
        errors
            |> List.map (Html.text >> List.singleton >> Html.li [])
            |> Html.ul []
            |> List.singleton


update : PositionFormMsg -> PositionForm -> PositionForm
update msg model =
    (case msg of
        UpdateDescription v ->
            { model | description = v }

        UpdateChampion v ->
            { model | champion = v }
    )
        |> updateErrors


updateErrors : PositionForm -> PositionForm
updateErrors model =
    { model
        | errors = validator model |> getErrors
    }


getErrors : Result (List a) b -> List a
getErrors r =
    case r of
        Ok _ ->
            []

        Err err ->
            err


toPosition : Participant -> PositionForm -> Result (List String) Position
toPosition participant form =
    validator form
        |> Result.map
            (\form ->
                { participant = participant
                , description = form.description
                , champion = form.champion
                }
            )


validator : V.Validator String PositionForm ValidForm
validator =
    V.ok ValidForm
        |> V.keep .orderId
        |> V.verify .description (VString.notBlank "Podaj opis zamówienia")
        |> V.keep .champion
