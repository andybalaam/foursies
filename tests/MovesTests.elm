module MovesTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Utils


import Board
import Moves


all : Test
all =
    describe "Tests of the move calculator"
        [ test "Normally all white pieces can move" allWhitePiecesCanMove
        , test "Normally all black pieces can move" allBlackPiecesCanMove
--        , test "Generate all moves with no jumps" allMovesNoJumps
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


--allMovesNoJumps : () -> Expect.Expectation
--allMovesNoJumps =
--    forBoard
--        "XXXX"
--        "...."
--        "...."
--        "OOOO"
--        <| \board -> Utils.equalExceptOrder
--            [ (Moves.slide (0, 3) (0, 2))
--            , (Moves.slide (0, 3) (1, 2))
--            , (Moves.slide (1, 3) (0, 2))
--            , (Moves.slide (1, 3) (1, 2))
--            , (Moves.slide (1, 3) (2, 2))
--            , (Moves.slide (2, 3) (1, 2))
--            , (Moves.slide (2, 3) (2, 2))
--            , (Moves.slide (2, 3) (3, 2))
--            , (Moves.slide (3, 3) (2, 2))
--            , (Moves.slide (3, 3) (3, 2))
--            ]
--            ( Moves.allowedMoves Board.whitePiece board )


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
