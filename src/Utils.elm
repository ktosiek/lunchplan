module Utils exposing (appendIf, getErrors)


appendIf : Bool -> List a -> List a -> List a
appendIf b new base =
    if b then
        base ++ new
    else
        base


getErrors : Result (List a) b -> List a
getErrors r =
    case r of
        Ok _ ->
            []

        Err err ->
            err
