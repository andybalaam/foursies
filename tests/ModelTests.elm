module ModelTests exposing (all)

import Test exposing (describe,test,Test)
import Expect

import Board
import Model

all : Test
all =
    describe "Tests of the model code"
     [ test "Opposite of X is O" oppositeOfXIsO
     , test "Opposite of O is X" oppositeOfOIsX
     , test "Convert side to piece" convertSideToPiece
     , test "Convert piece to side" convertPieceToSide
     ]


oppositeOfXIsO : () -> Expect.Expectation
oppositeOfXIsO =
    \() ->
        Expect.equal Model.OSide (Model.oppositeSide Model.XSide)


oppositeOfOIsX : () -> Expect.Expectation
oppositeOfOIsX =
    \() ->
        Expect.equal Model.XSide (Model.oppositeSide Model.OSide)


convertSideToPiece : () -> Expect.Expectation
convertSideToPiece =
    \() ->
        Expect.equal
            ( Model.sidePiece Model.XSide
            , Model.sidePiece Model.OSide
            )
            ( Board.XPiece
            , Board.OPiece
            )


convertPieceToSide : () -> Expect.Expectation
convertPieceToSide =
    \() ->
        Expect.equal
            ( Model.pieceSide Board.XPiece
            , Model.pieceSide Board.OPiece
            , Model.pieceSide Board.noPiece
            , Model.pieceSide Board.offBoard
            )
            ( Just Model.XSide
            , Just Model.OSide
            , Nothing
            , Nothing
            )
