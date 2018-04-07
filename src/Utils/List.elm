module Utils.List exposing (upsert, update)


upsert : (a -> Bool) -> a -> List a -> List a
upsert pred elem list =
    update pred (always <| Just elem) list


update : (a -> Bool) -> (Maybe a -> Maybe a) -> List a -> List a
update pred f list =
    let
        maybeAppend base rest =
            case f base of
                Nothing ->
                    rest

                Just elem ->
                    elem :: rest
    in
        case list of
            [] ->
                maybeAppend Nothing []

            x :: xs ->
                if pred x then
                    maybeAppend (Just x) xs
                else
                    x :: update pred f xs
