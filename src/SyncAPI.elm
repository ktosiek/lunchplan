port module SyncAPI
    exposing
        ( SyncData
        , sync
        , User
        , user
        , Order
        , order
        , updateOrder
        , Position
        , position
        , updatePosition
        )


port sync : (SyncData -> msg) -> Sub msg


port user : (User -> msg) -> Sub msg


port position : (Position -> msg) -> Sub msg


port order : (Order -> msg) -> Sub msg


port updateOrder : Order -> Cmd msg


port updatePosition : Position -> Cmd msg


type alias SyncData =
    { orders : List Order
    , users : List User
    , positions : List Position
    }


type alias Order =
    { id : Int
    , name : String
    , link : String
    , description : String
    , isOrdered : Bool
    }


type alias User =
    { id : String
    , name : String
    }


type alias Position =
    { orderId : Int
    , participantId : String
    , description : String
    , champion : Bool
    }
