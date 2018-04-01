module Types exposing (..)


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
