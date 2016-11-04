module BoardTests exposing (all)

import Test exposing (describe,test,Test)
import Expect

import Utils exposing (allEqual)
import Board


all : Test
all =
    describe "Tests of the game board parser and formatter"
        [ test "Parsing an empty board" parseEmptyBoard
        ]


parseEmptyBoard =
    \() ->
        let
            board = Board.parse
                "...."
                "...."
                "...."
                "...."
        in
            allEqual
                Board.noPiece
                (List.map (\x -> Board.pieceAt (x, 0) board) [0..2])
