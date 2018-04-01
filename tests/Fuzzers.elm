module Fuzzers exposing (..)

import Fuzz
import Fuzz exposing (Fuzzer)
import List.Extra as List
import Types exposing (..)


type alias Order =
    Types.Order


order : Fuzzer Order
order =
    Fuzz.map4 Order
        (Fuzz.int |> Fuzz.map OrderId)
        place
        positionsList
        orderStatus


orderStatus : Fuzzer OrderStatus
orderStatus =
    allOrderStatuses
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


place : Fuzzer Place
place =
    Fuzz.map3 Place Fuzz.string Fuzz.string Fuzz.string


positionsList : Fuzzer (List Position)
positionsList =
    Fuzz.list position
        |> Fuzz.map (List.uniqueBy (.participant >> .id >> unParticipantId))
        |> Fuzz.andThen
            (\ls ->
                if List.isEmpty ls then
                    position |> Fuzz.map List.singleton
                else
                    Fuzz.constant ls
            )


position : Fuzzer Position
position =
    Fuzz.map3 Position participant Fuzz.string Fuzz.bool


participant : Fuzzer Participant
participant =
    Fuzz.map2 Participant Fuzz.string (Fuzz.map ParticipantId Fuzz.string)


uniqueListBy : (a -> comparable) -> Fuzzer a -> Fuzzer (List a)
uniqueListBy key a =
    Fuzz.list a
        |> Fuzz.map (List.uniqueBy key)


unParticipantId (ParticipantId id) =
    id
