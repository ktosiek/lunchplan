module Sync exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Utils.List as List
import SyncAPI
import Synced exposing (Synced)
import OrderForm
import PositionForm
import Types


type alias SyncState =
    { participants : Dict String (Synced SyncAPI.User)
    , orders : Dict Int (Synced SyncAPI.Order)

    -- OrderId -> [Position]
    , positions : Dict Int (List (Synced SyncAPI.Position))
    }


emptyState : SyncState
emptyState =
    { participants = Dict.empty
    , orders = Dict.empty
    , positions = Dict.empty
    }


type alias SyncedModel a =
    { a
        | orders : List Types.Order
        , user : Types.Participant
        , syncState : SyncState
    }


applyFullSync : SyncAPI.SyncData -> SyncedModel a -> SyncedModel a
applyFullSync raw model =
    { participants =
        raw.users
            |> Dict.fromListBy .id
            |> Dict.map (\_ -> Synced.Synced)
    , positions =
        raw.positions
            |> Dict.groupBy .orderId
            |> Dict.map (\_ -> List.map Synced.Synced)
    , orders =
        raw.orders
            |> Dict.fromListBy .id
            |> Dict.map (\_ -> Synced.Synced)
    }
        |> flip denormalizeModel model


applyParticipant : SyncAPI.User -> SyncedModel a -> SyncedModel a
applyParticipant raw model =
    let
        syncState =
            model.syncState
    in
        { syncState | participants = Dict.insert raw.id (Synced.Synced raw) syncState.participants }
            |> flip denormalizeModel model


updatePosition : Types.Participant -> PositionForm.ValidForm -> SyncedModel a -> ( SyncedModel a, Cmd msg )
updatePosition participant form model =
    let
        syncState =
            model.syncState

        isTargetPosition =
            positionEq position

        addPosition =
            List.update
                (Synced.local >> isTargetPosition)
                (Synced.localFromMaybe position >> Just)

        position : SyncAPI.Position
        position =
            { orderId = Types.unOrderId form.orderId
            , participantId = Types.unParticipantId participant.id
            , champion = form.champion
            , description = form.description
            }
    in
        ({ syncState
            | positions =
                syncState.positions
                    |> Dict.update
                        (Types.unOrderId form.orderId)
                        (Maybe.withDefault [] >> addPosition >> Just)
         }
            |> flip denormalizeModel model
        )
            ! [ SyncAPI.updatePosition position ]


applyPosition : SyncAPI.Position -> SyncedModel a -> SyncedModel a
applyPosition raw model =
    let
        syncState =
            model.syncState

        addPosition =
            List.upsert (Synced.local >> positionEq raw) (Synced.Synced raw)
    in
        { syncState
            | positions =
                syncState.positions
                    |> Dict.update raw.orderId (Maybe.withDefault [] >> addPosition >> Just)
        }
            |> flip denormalizeModel model


updateOrder : Maybe Types.OrderId -> OrderForm.ValidForm -> SyncedModel a -> ( SyncedModel a, Cmd msg )
updateOrder morderId form model =
    let
        setOrder : Int -> Maybe (Synced SyncAPI.Order) -> Synced SyncAPI.Order
        setOrder orderId morder =
            { id = orderId, name = form.name, link = form.link, description = form.description }
                |> flip Synced.localFromMaybe morder

        syncState =
            model.syncState
    in
        ({ syncState
            | orders =
                flip Maybe.map
                    morderId
                    (\(Types.OrderId orderId) ->
                        Dict.update orderId
                            (setOrder orderId >> Just)
                            syncState.orders
                    )
                    |> Maybe.withDefault syncState.orders
         }
            |> flip denormalizeModel model
        )
            ! [ SyncAPI.updateOrder
                    { id = morderId |> Maybe.map Types.unOrderId |> Maybe.withDefault 0
                    , name = form.name
                    , link = form.link
                    , description = form.description
                    }
              ]


applyOrder : SyncAPI.Order -> SyncedModel a -> SyncedModel a
applyOrder rawOrder model =
    let
        syncState =
            model.syncState
    in
        { syncState
            | orders = Dict.insert rawOrder.id (Synced.Synced rawOrder) syncState.orders
        }
            |> flip denormalizeModel model


denormalizeModel : SyncState -> SyncedModel a -> SyncedModel a
denormalizeModel syncState model =
    { model
        | orders =
            syncState.orders
                |> Dict.values
                |> List.map Synced.local
                |> List.map
                    (\o ->
                        { id = Types.OrderId o.id
                        , place = { name = o.name, description = o.description, link = o.link }
                        , positions = getPositions syncState o.id
                        , status = Types.Proposed
                        }
                    )
                |> List.map Types.fixOrderStatus
        , user =
            getParticipant syncState (Types.unParticipantId model.user.id)
                |> Maybe.withDefault model.user
        , syncState = syncState
    }


loadPosition : SyncedModel a -> SyncAPI.Position -> Maybe Types.Position
loadPosition model rawPosition =
    getParticipant model.syncState rawPosition.participantId
        |> Maybe.map
            (\p ->
                { participant = p
                , description = rawPosition.description
                , champion = rawPosition.champion
                }
            )


getParticipant : SyncState -> String -> Maybe Types.Participant
getParticipant syncState participantId =
    Dict.get participantId syncState.participants
        |> Maybe.map Synced.local
        |> Maybe.map (\p -> { p | id = Types.ParticipantId p.id })


getPositions : SyncState -> Int -> List Types.Position
getPositions syncState orderId =
    Dict.get orderId syncState.positions
        |> Maybe.withDefault []
        |> List.map Synced.local
        |> List.filterMap
            (\position ->
                getParticipant syncState position.participantId
                    |> Maybe.map
                        (\participant ->
                            { participant = participant
                            , description = position.description
                            , champion = position.champion
                            }
                        )
            )


positionEq : SyncAPI.Position -> SyncAPI.Position -> Bool
positionEq p1 p2 =
    positionId p1 == positionId p2


type alias PositionId =
    ( Int, String )


positionId : SyncAPI.Position -> PositionId
positionId p =
    ( p.orderId, p.participantId )


newOrder : Int -> Types.Order
newOrder orderId =
    { id = Types.OrderId orderId
    , place =
        { name = ""
        , link = ""
        , description = ""
        }
    , status = Types.Proposed
    , positions = []
    }
