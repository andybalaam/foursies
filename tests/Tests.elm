module Tests exposing (all)


import Test exposing (describe,Test)


import BoardTests
import MovesTests
import UtilsTests


all : Test
all =
    describe "All tests"
        [ BoardTests.all
        , MovesTests.all
        , UtilsTests.all
        ]
