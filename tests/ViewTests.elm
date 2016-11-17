module ViewTests exposing (all)

import Test exposing (describe,test,Test)
import Expect

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Board
import Model
import Msg
import View


all : Test
all =
    describe "Tests of the view code"
        [ test "Ready to start" readyToStart
        ]


rtsHtml : Html Msg.Msg
rtsHtml =
    svg [ width "10", height "10" ] []


viewWithNewModel : Html Msg.Msg
viewWithNewModel =
    View.view (Model.newModel {height=10, width=10})


readyToStart : () -> Expect.Expectation
readyToStart =
    \() ->
        Expect.equal (toString rtsHtml) (toString viewWithNewModel)
