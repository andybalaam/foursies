module Tests exposing (all)


import Test exposing (describe,Test)


import BoardTests
import MovesTests
import UpdateTests
import UtilsTests
import ViewTests


all : Test
all =
    describe "All tests"
        [ BoardTests.all
        , MovesTests.all
        , UpdateTests.all
        , UtilsTests.all
        , ViewTests.all
        ]
