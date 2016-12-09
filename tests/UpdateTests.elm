module UpdateTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Mouse


import Model
import Msg
import Update exposing (update)


all : Test
all =
    describe "Tests for the update function"
        [ test "Resize updates model size" resizeUpdatesModelSize
        , test "Choosing player updates model" choosingPlayerUpdatesModel
        , test "Choosing other player updates model" choosingOtherPlayer
        , test "Changing X player updates" changingXPlayerUpdates
        , test "Changing O player updates" changingOPlayerUpdates
        , test "Impossible to have identical players" playersNotIdentical
        , test "Moving mouse updates" movingMouseUpdates
        , test "Start dragging a piece" startDragging
        , test "Stop dragging a piece without moving it" stopDraggingNoMove
        , test "Drop a piece where we can't move it" dropInBadPlace
        ]


modelEqual : Model.Model -> (Model.Model, Cmd Msg.Msg)
    -> (() -> Expect.Expectation)
modelEqual exp act =
    \() -> Expect.equal (exp, Cmd.none) act


resizeUpdatesModelSize : () -> Expect.Expectation
resizeUpdatesModelSize =
    modelEqual
        (Model.newModel {width=21, height=22})
        (update (Msg.Resize 21 22) (Model.newModel {width=1, height=14}))


choosingPlayerUpdatesModel : () -> Expect.Expectation
choosingPlayerUpdatesModel =
    let
        model = Model.newModel {width=10, height=10}
    in
        modelEqual
            { model | choosingSide = Just Model.XSide }
            (update (Msg.ChoosePlayer Model.XSide) model)


choosingOtherPlayer : () -> Expect.Expectation
choosingOtherPlayer =
    let
        model_ = Model.newModel {width=10, height=10}
        model = { model_ | choosingSide = Just Model.XSide }
    in
        modelEqual
            { model | choosingSide = Just Model.OSide }
            (update (Msg.ChoosePlayer Model.OSide) model)


changingXPlayerUpdates : () -> Expect.Expectation
changingXPlayerUpdates =
    let
        model_ = Model.newModel {width=10, height=10}
        model = { model_ | choosingSide = Just Model.XSide }
        chpl = model.chosenPlayers
    in
        modelEqual
            { model
            | choosingSide = Nothing
            , chosenPlayers = {chpl | x = Model.GreenPlayer } }
            (update (Msg.ChangePlayer Model.XSide Model.GreenPlayer) model)


changingOPlayerUpdates : () -> Expect.Expectation
changingOPlayerUpdates =
    let
        model_ = Model.newModel {width=10, height=10}
        model = { model_ | choosingSide = Just Model.OSide }
        chpl = model.chosenPlayers
    in
        modelEqual
            { model
            | choosingSide = Nothing
            , chosenPlayers = {chpl | o = Model.BluePlayer } }
            (update (Msg.ChangePlayer Model.OSide Model.BluePlayer) model)


playersNotIdentical : () -> Expect.Expectation
playersNotIdentical =
    let
        model_ = Model.newModel {width=10, height=10}
        model =
            { model_
            | choosingSide = Just Model.OSide
            , chosenPlayers = { x = Model.GreenPlayer, o = Model.BlackPlayer }
            }
    in
        -- chosenPlayers didn't change because this change is illegal
        modelEqual
            { model | choosingSide = Nothing }
            (update (Msg.ChangePlayer Model.XSide Model.BlackPlayer) model)


movingMouseUpdates : () -> Expect.Expectation
movingMouseUpdates =
    let
        model = Model.newModel {width=10, height=10}
    in
        modelEqual
            { model | mousePos = Mouse.Position 45 76 }
            (update (Msg.MouseMove (Mouse.Position 45 76)) model)


startDragging : () -> Expect.Expectation
startDragging =
    let
        model_ = Model.newModel {width=10, height=10}
        model = { model_ | mousePos = Mouse.Position 21 23 }
    in
        modelEqual
            { model
                | dragging = Just
                    <| Model.DragState 1 0 (Mouse.Position 21 23)
            }
            (update (Msg.DragStart 1 0) model)


stopDraggingNoMove : () -> Expect.Expectation
stopDraggingNoMove =
    let
        model_ = Model.newModel {width=10, height=10}
        model =
            { model_
            | mousePos = Mouse.Position 21 23
            , dragging = Just <| Model.DragState 2 2 (Mouse.Position 21 23)
            , message = Model.MessageMoveNotAllowed
            }
    in
        modelEqual
            { model
            | dragging = Nothing
            , message = Model.MessageNormal
            }
            (update Msg.DragStop model)


dropInBadPlace : () -> Expect.Expectation
dropInBadPlace =
    let
        model_ = Model.newModel {width=100, height=100}
        model =
            { model_
            | mousePos = Mouse.Position 21 203
            , dragging = Just <| Model.DragState 2 0 (Mouse.Position 5 5)
            }
    in
        modelEqual
            { model
            | dragging = Nothing
            , message = Model.MessageMoveNotAllowed
            }
            (update Msg.DragStop model)
