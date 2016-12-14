module UpdateTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Mouse


import Model
import Moves
import Msg
import Update exposing (update)
import Utils


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
        , test "Tap to start moving a piece" tapToStartMoving
        , test "Tap to finish moving a piece" tapToFinishMoving
        -- TODO: tap to cancel moving
        , test "Stop dragging a piece without moving it" stopDraggingNoMove
        , test "Drop a piece where we can't move it" dropInBadPlace
        , test "Drag to make a valid move" dragToMakeAValidMove
        , test "dragMoveAllowed picks right move" dragMoveAllowedPicksRightMove
        , test "Reset sets the board back" resetSetsTheBoardBack
        ]


basicModel : Model.Model
basicModel = Model.newModel {width=100, height=100}


modelEqual : Model.Model -> (Model.Model, Cmd Msg.Msg)
    -> (() -> Expect.Expectation)
modelEqual exp act =
    \() -> doModelEqual exp act


doModelEqual : Model.Model -> (Model.Model, Cmd Msg.Msg)
    -> (Expect.Expectation)
doModelEqual exp act =
    Expect.equal (exp, Cmd.none) act


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


tapToStartMoving : () -> Expect.Expectation
tapToStartMoving =
    let
        model = Model.newModel {width=10, height=10}
    in
        modelEqual
            { model | dragging = Just <| Model.TouchedState 1 0 }
            (update (Msg.Touched 1 0) model)


tapToFinishMoving : () -> Expect.Expectation
tapToFinishMoving =
    let
        model_ = Model.newModel {width=10, height=10}
        model =
            { model_ | dragging = Just <| Model.TouchedState 1 0 }
    in
        Utils.forBoard
            "X.XX"
            ".X.."
            "...."
            "OOOO" <| \board ->
                doModelEqual
                    { model
                    | dragging = Nothing
                    , board = board
                    , turn = Model.OSide
                    }
                    (update (Msg.Touched 1 1) model)


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
        model =
            { basicModel
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


-- Make a slide from 1,0 to 1,1 and don't have the bug I had
-- where it picked 0,0 to 1,1 just because it was happy
-- with anything ending at 1,1
dragMoveAllowedPicksRightMove : () -> Expect.Expectation
dragMoveAllowedPicksRightMove =
    \() ->
        Expect.equal
            (Update.YesAllowed <| Moves.Slide (1, 0) (1, 1))
            (Update.dragMoveAllowed
                { basicModel
                | mousePos = Mouse.Position 5 23
                , dragging = Just <| Model.DragState 1 0 (Mouse.Position 5 5)
                }
                1
                0
                (Mouse.Position 5 5)
            )


-- Drag from 1,0 to 1,1 and don't have the bug I had where it decided to
-- pretend it was really 0,0 to 1,1
dragToMakeAValidMove : () -> Expect.Expectation
dragToMakeAValidMove =
    Utils.forBoard
        "XXXX"
        "...."
        "...."
        "OOOO" <| \startBoard ->
            Utils.doForBoard
                "X.XX"
                ".X.."
                "...."
                "OOOO" <| \endBoard ->
                    Expect.equal
                        (
                            { basicModel
                            | mousePos = Mouse.Position 5 23
                            , dragging = Nothing
                            , message = Model.MessageNormal
                            , board = endBoard
                            , turn = Model.OSide
                            }
                        , Cmd.none
                        )
                        (update Msg.DragStop
                            { basicModel
                            | mousePos = Mouse.Position 5 23
                            , dragging = Just <|
                                Model.DragState 1 0 (Mouse.Position 5 5)
                            , board = startBoard
                            }
                        )


resetSetsTheBoardBack : () -> Expect.Expectation
resetSetsTheBoardBack =
    Utils.forBoard
        ".X.X"
        "X.X."
        ".O.."
        "...." <| \startBoard ->
            Expect.equal
                (
                    { basicModel
                    | message = Model.MessageNormal
                    , turn = Model.XSide
                    }
                , Cmd.none
                )
                (update Msg.StartAgain
                    { basicModel
                    | board = startBoard
                    , message = Model.MessageMoveNotAllowed
                    , turn = Model.OSide
                    }
                )


winningMoveUpdatesMessage : () -> Expect.Expectation
winningMoveUpdatesMessage =
    let
        model =
            { basicModel
            | dragging = Just <| Model.TouchedState 0 1
            }
    in
        \() ->
            Expect.equal
                (Model.MessageWon Model.OSide)
                (messageOf <| update (Msg.Touched 0 0) model)

--

messageOf : (Model.Model, Cmd.Cmd Msg.Msg) -> Model.Message
messageOf (model, _) =
    model.message
