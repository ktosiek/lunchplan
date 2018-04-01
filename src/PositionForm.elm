module PositionForm exposing (..)

import Model exposing (..)
import Utils exposing (..)
import Html exposing (..)
import Html.Attributes exposing (type_, value, checked, placeholder)
import Html.Events exposing (onInput, onCheck, onClick)
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Verify as V
import String.Verify as VString


type alias ValidForm =
    { orderId : OrderId
    , description : String
    , champion : Bool
    }


type alias FieldError =
    ( PositionFormField, String )


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


view : (PositionFormMsg -> msg) -> msg -> PositionForm -> List (Html msg)
view send saveMsg form =
    Form.form [ Html.Events.onSubmit saveMsg ]
        ([ Form.group []
            [ Form.label [] [ text "Zamawiane danie" ]
            , Input.text
                ([ Input.attrs
                    [ onInput (UpdateDescription >> send)
                    , value form.description
                    , placeholder "dkkFM 📻"
                    ]
                 ]
                    |> appendIf (not <| List.isEmpty <| fieldErrors Description form)
                        [ Input.danger ]
                )
            , showErrors form.errors
            ]
         , Form.group []
            ([ Checkbox.checkbox
                [ Checkbox.id "champion"
                , Checkbox.attrs [ checked form.champion, onCheck (UpdateChampion >> send) ]
                ]
                "Złożę zamówienie"
             ]
            )
         ]
            ++ [ Button.button [ Button.primary, Button.attrs [] ] [ text "Zapisz" ]
               ]
        )
        |> List.singleton


showErrors : List FieldError -> Html msg
showErrors errors =
    Form.invalidFeedback [] <|
        if List.length errors == 0 then
            []
        else
            errors
                |> List.map (Tuple.second >> Html.text >> List.singleton >> Html.li [])
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


fieldErrors : PositionFormField -> PositionForm -> List String
fieldErrors field model =
    model.errors
        |> List.filterMap
            (\( f, err ) ->
                if f == field then
                    Just err
                else
                    Nothing
            )


toPosition : Participant -> PositionForm -> Result (List FieldError) Position
toPosition participant form =
    validator form
        |> Result.map
            (\form ->
                { participant = participant
                , description = form.description
                , champion = form.champion
                }
            )


validator : V.Validator FieldError PositionForm ValidForm
validator =
    V.ok ValidForm
        |> V.keep .orderId
        |> V.verify .description (VString.notBlank ( Description, "Podaj opis zamówienia" ))
        |> V.keep .champion