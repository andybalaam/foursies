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
        , test "Enumerating all positions" enumerateAllPositions
        ]


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
                        (List.map (\x -> Board.pieceAt (x, 0) b) [0..2])


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
