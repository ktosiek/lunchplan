module Utils exposing (..)


appendIf : Bool -> List a -> List a -> List a
appendIf b new base =
    if b then
        base ++ new
    else
        base
