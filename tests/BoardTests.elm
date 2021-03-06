module BoardTests exposing (all)

import Test exposing (describe,test,Test)
import Expect

import Utils exposing (allEqual)
import Board


all : Test
all =
    describe "Tests of the game board parser and formatter"
        [ test "Parsing an empty board" parseEmptyBoard
        , test "Parsing a board with a long row" parseBoardWithLongRow
        , test "Parsing a board with a long row" parseBoardWithShortRow
        , test "Parsing a board with a bad char" parseBoardWithInvalidChar
        , test "Parsing + formatting a non-empty board" roundTripRandomBoard
        , test "Create a board programmatically" createABoard
        , test "Enumerating all positions" enumerateAllPositions
        , describe "pieceAt for various positions" piecesAtVariousPositions
        ]


createABoard : () -> Expect.Expectation
createABoard =
    \() ->
        let
            board1 = Board.newBoard
                Board.xPiece  Board.noPiece Board.oPiece  Board.noPiece
                Board.noPiece Board.xPiece  Board.noPiece Board.oPiece
                Board.xPiece  Board.noPiece Board.oPiece  Board.noPiece
                Board.noPiece Board.xPiece  Board.noPiece Board.oPiece
            board2 = Board.parse <| Board.strings
                "X.O."
                ".X.O"
                "X.O."
                ".X.O"
        in
            case board2 of
                Err e -> Expect.fail e
                Ok b ->
                    Expect.equal board1 b


parseEmptyBoard : () -> Expect.Expectation
parseEmptyBoard =
    \() ->
        let
            input = Board.strings
                "...."
                "...."
                "...."
                "...."
            board = Board.parse input
        in
            case board of
                Err e -> Expect.fail e
                Ok b ->
                    allEqual
                        Board.noPiece
                        (List.map
                            (\x -> Board.pieceAt (x, 0) b)
                            (List.range 0 2))


parseBoardWithLongRow : () -> Expect.Expectation
parseBoardWithLongRow =
    \() ->
        let
            input = Board.strings
                "...."
                "..X.."
                "...."
                "...."
        in
            Expect.equal
                (Err "Line does not contain 4 characters (line 2)")
                (Board.parse input)


parseBoardWithShortRow : () -> Expect.Expectation
parseBoardWithShortRow =
    \() ->
        let
            input = Board.strings
                "..."
                "...."
                "X..."
                "...."
        in
            Expect.equal
                (Err "Line does not contain 4 characters (line 1)")
                (Board.parse input)


parseBoardWithInvalidChar : () -> Expect.Expectation
parseBoardWithInvalidChar =
    \() ->
        let
            input = Board.strings
                "...."
                "...."
                "X..."
                "O.a."
        in
            Expect.equal
                (Err ("Invalid character 'a' - " ++
                    "expected '.', 'X', or 'O' (char 3) (line 4)"))
                (Board.parse input)


roundTripRandomBoard : () -> Expect.Expectation
roundTripRandomBoard =
    \() ->
        let
            input = Board.strings
                "X.O."
                "...."
                ".XO."
                "...."
            board = Board.parse input
        in
            case board of
                Err e -> Expect.fail e
                Ok  b -> Expect.equal input (Board.toStrings b)


enumerateAllPositions : () -> Expect.Expectation
enumerateAllPositions =
    \() ->
        Expect.equal
            [ (0, 0)
            , (1, 0)
            , (2, 0)
            , (3, 0)
            , (0, 1)
            , (1, 1)
            , (2, 1)
            , (3, 1)
            , (0, 2)
            , (1, 2)
            , (2, 2)
            , (3, 2)
            , (0, 3)
            , (1, 3)
            , (2, 3)
            , (3, 3)
            ]
            Board.positions


piecesAtVariousPositions : List Test
piecesAtVariousPositions =
    let
        fb = Utils.forBoard
            "X..."
            "...."
            ".O.."
            "...."
        t = \msg pos expectedPiece ->
            test msg (
                fb <| \b -> Expect.equal expectedPiece <| Board.pieceAt pos b
                )
    in
        [ t "Top left is X" (0,0) Board.xPiece
        , t "(1, 2) is O" (1,2) Board.oPiece
        , t "Top right is nothing" (3,0) Board.noPiece
        , t "Bottom right is nothing" (3,3) Board.noPiece
        , t "Off board" (4,3) Board.offBoard
        ]
