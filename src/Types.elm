module Types
    exposing
        ( Order
        , OrderStatus(..)
        , allOrderStatuses
        , OrderId(..)
        , unOrderId
        , fixOrderStatus
        , Place
        , Position
        , Participant
        , ParticipantId(..)
        , unParticipantId
        , isChampioning
        )


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


fixOrderStatus : Order -> Order
fixOrderStatus order =
    { order
        | status =
            case order.status of
                Ordered ->
                    Ordered

                _ ->
                    if List.any .champion order.positions then
                        Championed
                    else
                        Proposed
    }


unParticipantId : ParticipantId -> String
unParticipantId (ParticipantId id) =
    id


unOrderId : OrderId -> Int
unOrderId (OrderId i) =
    i


isChampioning : Participant -> Order -> Bool
isChampioning participant order =
    let
        anyChampions =
            order.positions
                |> List.any (\p -> p.participant.id == participant.id)
    in
        order.status /= Ordered && anyChampions
