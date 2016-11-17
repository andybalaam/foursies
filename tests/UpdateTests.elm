module UpdateTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Model
import Msg
import Update exposing (update)


all : Test
all =
    describe "Tests for the update function"
        [ test "Resize updates model size" resizeUpdatesModelSize
        ]


resizeUpdatesModelSize : () -> Expect.Expectation
resizeUpdatesModelSize =
    \() ->
        Expect.equal
            ((Model.newModel {width=21, height=22}), Cmd.none)
            (update (Msg.Resize 21 22) (Model.newModel {width=1, height=14}) )
