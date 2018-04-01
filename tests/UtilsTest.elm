module UtilsTest exposing (..)

import Debug
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html
import List.Extra as List
import Test exposing (..)
import Fuzzers
import Utils exposing (..)


suite : Test
suite =
    describe "Utils"
        [ describe "appendIf"
            [ test "True"
                (\_ -> [ Expect.equal [ 1, 2, 3 ] (appendIf True [ 1 ] [ 2, 3 ]) ])
            , test "False"
                (\_ -> [ Expect.equal [ 1 ] (appendIf False [ 1 ] [ 2, 3 ]) ])
            ]
        ]
