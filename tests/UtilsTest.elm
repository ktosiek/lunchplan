module UtilsTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Utils exposing (..)


suite : Test
suite =
    describe "Utils"
        [ describe "appendIf"
            [ test "True"
                (\_ -> Expect.equal [ 1, 2, 3 ] (appendIf True [ 2, 3 ] [ 1 ]))
            , test "False"
                (\_ -> Expect.equal [ 1 ] (appendIf False [ 2, 3 ] [ 1 ]))
            ]
        , describe "getErrors"
            [ test "Err" (\_ -> Expect.equal [ "x" ] <| getErrors (Err [ "x" ]))
            , test "Ok" (\_ -> Expect.equal [] <| getErrors (Ok [ "x" ]))
            ]
        ]
