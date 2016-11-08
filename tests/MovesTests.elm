module MovesTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Board
import Moves


all : Test
all =
    describe "Tests of the move calculator"
        [ test "Normally all pieces can move" normallyAllPiecesCanMove
        ]


normallyAllPiecesCanMove : () -> Expect.Expectation
normallyAllPiecesCanMove =
    \() ->
        let
            board = Board.parse <| Board.strings
                "X..."
                "...."
                "...."
                "O.O."
        in
            case board of
                Err e -> Expect.fail e
                Ok b ->
                    Expect.equal
                        [ (0, 3)
                        , (2, 3)
                        ]
                        <| Moves.whichCanMove Board.whitePiece b
