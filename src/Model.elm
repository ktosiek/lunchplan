module Model exposing (..)

import Bootstrap.Navbar as Navbar


type alias Model =
    { navbar : Navbar.State
    , orders : List Order
    , user : Participant
    , positionForm : Maybe PositionForm
    }


type alias PositionForm =
    { orderId : OrderId
    , description : String
    , champion : Bool
    , errors : List ( PositionFormField, String )
    }


type PositionFormField
    = Description


type OrderStatus
    = Proposed
    | Championed
    | Ordered


allOrderStatuses : List OrderStatus
allOrderStatuses =
    [ Ordered, Championed, Proposed ]


type alias Order =
    { id : OrderId
    , place : Place
    , positions : List Position
    , status : OrderStatus
    }


type OrderId
    = OrderId Int


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
    | OpenPositionForm OrderId
    | UpdatePositionForm PositionFormMsg
    | SavePositionForm


type PositionFormMsg
    = UpdateDescription String
    | UpdateChampion Bool
