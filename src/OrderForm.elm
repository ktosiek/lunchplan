module OrderForm exposing (Model, ValidForm, Msg, view, validator, update, newOrder, fromOrder)

import Utils exposing (appendIf, getErrors)
import Types exposing (OrderId)
import Html exposing (Html, text)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes as A
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Verify as V exposing (Validator)
import String.Verify as VString
import String


type alias Model =
    { orderId : Maybe OrderId
    , name : String
    , link : String
    , description : String
    , errors : List ( FormField, String )
    }


type FormField
    = Name
    | Description
    | Link


fieldInfo : FormField -> { name : String, placeholder : String, msg : String -> Msg }
fieldInfo field =
    case field of
        Name ->
            { name = "Nazwa", placeholder = "Za rogiem", msg = UpdateName }

        Description ->
            { name = "Opis", placeholder = "Tu blisko, tylko kawałek dalej", msg = UpdateDescription }

        Link ->
            { name = "Link", placeholder = "https://zarogiem.example.com/", msg = UpdateLink }


type Msg
    = UpdateDescription String
    | UpdateName String
    | UpdateLink String


type alias ValidForm =
    { name : String
    , link : String
    , description : String
    }


type alias FieldError =
    ( FormField, String )


fromOrder : Types.Order -> Model
fromOrder order =
    { orderId = Just order.id
    , name = order.place.name
    , link = order.place.link
    , description = order.place.description
    , errors = []
    }
        |> updateErrors


newOrder : Model
newOrder =
    { orderId = Nothing
    , name = ""
    , link = ""
    , description = ""
    , errors = []
    }
        |> updateErrors


view : (Msg -> msg) -> msg -> Model -> List (Html msg)
view send saveMsg form =
    Form.form [ onSubmit saveMsg ]
        [ fieldWrapper inputField form Name |> Html.map send
        , fieldWrapper inputField form Link |> Html.map send
        , fieldWrapper inputField form Description |> Html.map send
        , Button.button [ Button.primary ] [ text "Zapisz" ]
        ]
        |> List.singleton


update : Msg -> Model -> Model
update msg model =
    (case msg of
        UpdateName v ->
            { model | name = v }

        UpdateLink v ->
            { model | link = v }

        UpdateDescription v ->
            { model | description = v }
    )
        |> updateErrors


updateErrors : Model -> Model
updateErrors model =
    { model
        | errors = validator model |> getErrors
    }


validator : Validator FieldError Model ValidForm
validator =
    V.ok ValidForm
        |> V.verify .name (VString.notBlank ( Name, "Podaj nazwę dostawcy" ))
        |> V.verify .link (isURL ( Link, "Podaj prawidłowy link" ) |> orEmpty)
        |> V.keep .description


isURL : err -> Validator err String String
isURL err str =
    if String.startsWith "http://" str || String.startsWith "https://" str then
        Result.Ok str
    else
        Result.Err [ err ]


orEmpty : Validator err String String -> Validator err String String
orEmpty validator str =
    if String.isEmpty str then
        Ok str
    else
        validator str


fieldWrapper : (String -> String -> (String -> Msg) -> Bool -> Html Msg) -> Model -> FormField -> Html Msg
fieldWrapper field form formField =
    let
        { name, placeholder, msg } =
            fieldInfo formField

        value =
            fieldValue formField form

        errors =
            fieldErrors formField form
    in
        Form.group []
            [ Form.label [] [ text name ]
            , field placeholder value msg (errors /= [])
            , showErrors errors
            ]


inputField : String -> String -> (String -> Msg) -> Bool -> Html Msg
inputField placeholder_ value_ msg hasErrors =
    Input.text
        ([ Input.attrs
            [ onInput msg
            , A.value value_
            , A.placeholder placeholder_
            ]
         ]
            |> appendIf hasErrors [ Input.danger ]
        )


fieldValue : FormField -> Model -> String
fieldValue field model =
    case field of
        Name ->
            model.name

        Link ->
            model.link

        Description ->
            model.description


fieldErrors : FormField -> Model -> List String
fieldErrors field model =
    model.errors
        |> List.filterMap
            (\( f, err ) ->
                if f == field then
                    Just err
                else
                    Nothing
            )


showErrors : List String -> Html msg
showErrors errors =
    Form.invalidFeedback [] <|
        if List.length errors == 0 then
            []
        else
            errors
                |> List.map (Html.text >> List.singleton >> Html.li [])
                |> Html.ul []
                |> List.singleton
