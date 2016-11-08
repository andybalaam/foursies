module Tests exposing (all)

import Test exposing (describe,Test)

import BoardTests
import MovesTests

all : Test
all =
    describe "All tests"
        [ BoardTests.all
        , MovesTests.all
        ]
