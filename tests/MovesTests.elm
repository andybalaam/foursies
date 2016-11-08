module MovesTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Board
import Moves


all : Test
all =
    describe "Tests of the move calculator"
        [ test "Normally all white pieces can move" allWhitePiecesCanMove
        , test "Normally all black pieces can move" allBlackPiecesCanMove
        ]


allWhitePiecesCanMove : () -> Expect.Expectation
allWhitePiecesCanMove =
    forBoard
        "X..."
        "...."
        "...."
        "O.O."
        <| \board -> Expect.equal
            [(0, 3), (2, 3)]
            ( Moves.whichCanMove Board.whitePiece board )


allBlackPiecesCanMove : () -> Expect.Expectation
allBlackPiecesCanMove =
    forBoard
        "...."
        ".X.."
        "...."
        "O.O."
        <| \board -> Expect.equal
            [(1, 1)]
            ( Moves.whichCanMove Board.blackPiece board )


-- Helpers


forBoard :
    String -> String -> String -> String ->
        (Board.Board -> Expect.Expectation) ->
        (() -> Expect.Expectation)
forBoard s1 s2 s3 s4 fn =
    \() ->
        let
            board = Board.parse <| Board.strings s1 s2 s3 s4
        in
            case board of
                Err e -> Expect.fail e
                Ok b -> fn b
