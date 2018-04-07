module UtilsTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Utils exposing (..)
import Utils.List exposing (upsert, update)


suite : Test
suite =
    Test.concat [ utilsSuite, utilsListSuite ]


utilsSuite : Test
utilsSuite =
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


utilsListSuite : Test
utilsListSuite =
    describe "Utils.List"
        [ describe "upsert"
            [ test "upsert: update" (\_ -> upsert ((==) 2) 0 [ 1, 2, 3 ] |> Expect.equal [ 1, 0, 3 ])
            , test "upsert: insert" (\_ -> upsert ((==) 2) 0 [ 1, 3 ] |> Expect.equal [ 1, 3, 0 ])
            , test "upsert: insert into empty" (\_ -> upsert ((==) 2) 0 [] |> Expect.equal [ 0 ])
            ]
        , describe "update"
            [ test "update: update"
                (\_ ->
                    update ((==) 2) (Maybe.map (\x -> x + 1)) [ 1, 2, 3 ]
                        |> Expect.equal [ 1, 3, 3 ]
                )
            , test "update: insert"
                (\_ ->
                    update ((==) 0) (always (Just 5)) [ 1, 2, 3 ]
                        |> Expect.equal [ 1, 2, 3, 5 ]
                )
            ]
        ]
