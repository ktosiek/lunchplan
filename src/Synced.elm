module Synced exposing (..)


type Synced a
    = Synced a
    | Saving a a
    | Creating a


map : (a -> a) -> Synced a -> Synced a
map f v =
    case v of
        Synced a ->
            Synced (f a)

        Saving remote local ->
            Saving remote (f local)

        Creating a ->
            Creating (f a)


localFromMaybe : a -> Maybe (Synced a) -> Synced a
localFromMaybe local mbase =
    case mbase of
        Nothing ->
            Creating local

        Just (Synced a) ->
            Saving a local

        Just (Saving remote _) ->
            Saving remote local

        Just (Creating _) ->
            Creating local


local : Synced a -> a
local v =
    case v of
        Synced a ->
            a

        Saving _ a ->
            a

        Creating a ->
            a


remote : Synced a -> Maybe a
remote v =
    case v of
        Synced a ->
            Just a

        Saving a _ ->
            Just a

        Creating a ->
            Nothing


mapRemote : (a -> a) -> Synced a -> Synced a
mapRemote f v =
    case v of
        Synced a ->
            Synced (f a)

        Saving remote local ->
            Saving (f remote) local |> normalize

        Creating a ->
            v


normalize : Synced a -> Synced a
normalize v =
    case v of
        Synced a ->
            v

        Saving r l ->
            if r == l then
                Synced l
            else
                v

        Creating a ->
            v
