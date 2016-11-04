module Tests exposing (all)

import Test exposing (describe,Test)

import BoardTests

all : Test
all =
    describe "All tests"
        [ BoardTests.all
        ]
