module Synced
    exposing
        ( Synced(..)
        , mapLocal
        , localFromMaybe
        , local
        , remote
        )


type Synced a
    = Synced a
    | Saving a a
    | Creating a


mapLocal : (a -> a) -> Synced a -> Synced a
mapLocal f s =
    (case s of
        Synced l ->
            Saving l (f l)

        Saving remote local ->
            Saving remote (f local)

        Creating l ->
            Creating (f l)
    )
        |> normalize


localFromMaybe : a -> Maybe (Synced a) -> Synced a
localFromMaybe local mbase =
    (case mbase of
        Nothing ->
            Creating local

        Just (Synced a) ->
            Saving a local

        Just (Saving remote _) ->
            Saving remote local

        Just (Creating _) ->
            Creating local
    )
        |> normalize


local : Synced a -> a
local s =
    case s of
        Synced a ->
            a

        Saving _ a ->
            a

        Creating a ->
            a


remote : Synced a -> Maybe a
remote s =
    case s of
        Synced a ->
            Just a

        Saving a _ ->
            Just a

        Creating _ ->
            Nothing


normalize : Synced a -> Synced a
normalize s =
    case s of
        Synced _ ->
            s

        Saving r l ->
            if r == l then
                Synced l
            else
                s

        Creating _ ->
            s
