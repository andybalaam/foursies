module ModelTests exposing (all)

import Test exposing (describe,test,Test)
import Expect

import Model

all : Test
all =
    describe "Tests of the model code"
     [ test "Opposite of X is O" oppositeOfXIsO
     , test "Opposite of O is X" oppositeOfOIsX
     ]


oppositeOfXIsO : () -> Expect.Expectation
oppositeOfXIsO =
    \() ->
        Expect.equal Model.OSide (Model.oppositeSide Model.XSide)


oppositeOfOIsX : () -> Expect.Expectation
oppositeOfOIsX =
    \() ->
        Expect.equal Model.XSide (Model.oppositeSide Model.OSide)
