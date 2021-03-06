module Sync
    exposing
        ( SyncState
        , emptyState
        , applyFullSync
        , applyOrder
        , updateOrder
        , orderOrder
        , applyParticipant
        , applyPosition
        , updatePosition
        )

import Dict exposing (Dict)
import Dict.Extra as Dict
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
        syncState =
            model.syncState

        baseOrder =
            morderId
                |> Maybe.map Types.unOrderId
                |> Maybe.andThen (flip Dict.get syncState.orders)

        localOrder orderId =
            setOrder orderId baseOrder

        setOrder : Int -> Maybe (Synced SyncAPI.Order) -> Synced SyncAPI.Order
        setOrder orderId morder =
            { id = orderId
            , name = form.name
            , link = form.link
            , description = form.description
            , isOrdered = Maybe.map (Synced.local >> .isOrdered) morder |> Maybe.withDefault False
            }
                |> flip Synced.localFromMaybe morder
    in
        ({ syncState
            | orders =
                flip Maybe.map
                    morderId
                    (\(Types.OrderId orderId) ->
                        Dict.insert orderId (localOrder orderId) syncState.orders
                    )
                    |> Maybe.withDefault syncState.orders
         }
            |> flip denormalizeModel model
        )
            ! [ SyncAPI.updateOrder
                    (morderId
                        |> Maybe.map Types.unOrderId
                        |> Maybe.withDefault 0
                        |> localOrder
                        |> Synced.local
                    )
              ]


orderOrder : Types.OrderId -> SyncedModel a -> ( SyncedModel a, Cmd msg )
orderOrder orderId model =
    let
        syncState =
            model.syncState

        rawId =
            Types.unOrderId orderId

        ( newOrders, cmd ) =
            syncState.orders
                |> updateWith rawId
                    (Synced.mapLocal (\o -> { o | isOrdered = True })
                        >> (\order -> ( order, SyncAPI.updateOrder (Synced.local order) ))
                    )
    in
        ( denormalizeModel { syncState | orders = newOrders } model, cmd )


updateWith : comparable -> (a -> ( a, Cmd msg )) -> Dict comparable a -> ( Dict comparable a, Cmd msg )
updateWith id f d =
    Dict.get id d
        |> Maybe.map f
        |> Maybe.map (\( v, cmd ) -> ( Dict.insert id v d, cmd ))
        |> Maybe.withDefault ( d, Cmd.none )


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
                        , status =
                            if o.isOrdered then
                                Types.Ordered
                            else
                                Types.Proposed
                        }
                    )
                |> List.map Types.fixOrderStatus
        , user =
            getParticipant syncState (Types.unParticipantId model.user.id)
                |> Maybe.withDefault model.user
        , syncState = syncState
    }


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
