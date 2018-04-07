module TypesTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Types exposing (..)


type alias Order =
    Types.Order


suite : Test
suite =
    describe "fixOrderStatus"
        [ test "Proposed -> Championed"
            (\_ ->
                { baseOrder
                    | positions =
                        [ { champion = True
                          , description = "Dej żryć"
                          , participant = baseParticipant
                          }
                        ]
                }
                    |> fixOrderStatus
                    |> .status
                    |> Expect.equal Championed
            )
        , test "Ordered"
            (\_ ->
                { baseOrder
                    | positions =
                        [ { champion = True
                          , description = "Dej żryć"
                          , participant = baseParticipant
                          }
                        ]
                    , status = Ordered
                }
                    |> fixOrderStatus
                    |> .status
                    |> Expect.equal Ordered
            )
        ]


baseOrder : Order
baseOrder =
    { place = { name = "Testowe", link = "", description = "Super miejsce do testów" }
    , status = Proposed
    , positions = []
    , id = OrderId 42
    }


baseParticipant : Participant
baseParticipant =
    { name = "Jakiś kolo"
    , id = ParticipantId "koloid"
    }
