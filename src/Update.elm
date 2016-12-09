module Update exposing (update)


import Board
import Model
import Moves
import Msg
import PixelScale


update : Msg.Msg -> Model.Model -> (Model.Model, Cmd Msg.Msg)
update msg model =
    let m =
        case msg of
            Msg.Resize w h -> updateResize w h model
            Msg.ChoosePlayer side -> { model | choosingSide = Just side }
            Msg.ChangePlayer side player -> updatePlayer model side player
            Msg.MouseMove pos -> { model | mousePos = pos }
            Msg.DragStart xpos ypos -> updateDragStart xpos ypos model
            Msg.DragStop -> updateDragStop model
    in
        (m, Cmd.none)


type MoveAllowed = YesAllowed Moves.Move | NotAllowed Model.Message


moveAllowed : Model.Model -> MoveAllowed
moveAllowed model =
    case model.dragging of
        Nothing -> NotAllowed Model.MessageNormal
        Just (Model.DragState xpos ypos startPx) ->
            let
                (moveX, moveY) =
                    PixelScale.gridDistance model startPx model.mousePos
            in
                if moveX == 0 && moveY == 0 then
                    NotAllowed Model.MessageNormal
                else
                    let
                        endx = xpos + moveX
                        endy = ypos + moveY
                        thisMove = List.filter
                            (\move -> Moves.to move == (endx, endy))
                            (Moves.allowedMoves
                                (Model.sidePiece model.turn) model.board)
                    in
                        case List.head thisMove of
                            Nothing -> NotAllowed Model.MessageMoveNotAllowed
                            Just move -> YesAllowed move


updateDragStop : Model.Model -> Model.Model
updateDragStop model =
    let
        allowed = moveAllowed model
    in
        case allowed of
            YesAllowed move ->
                { model
                | dragging = Nothing
                , message = Model.MessageNormal
                , board = Moves.movePiece model.board move
                , turn = Model.oppositeSide model.turn
                }
            NotAllowed message ->
                { model
                | dragging = Nothing
                , message = message
                }


updateDragStart : Int -> Int -> Model.Model -> Model.Model
updateDragStart xpos ypos model =
    { model
        | dragging = Just <| Model.DragState xpos ypos model.mousePos
    }


updateResize : Int -> Int -> Model.Model -> Model.Model
updateResize w h model =
    let
        scr = model.screen
    in
        {model | screen = {scr |
            width = w,
            height = h
        }}


oppositePlayer : Model.Model -> Model.Side -> Model.Player
oppositePlayer model side =
    Model.sidePlayer model (Model.oppositeSide side)


updatePlayer : Model.Model -> Model.Side -> Model.Player -> Model.Model
updatePlayer model side player =
    let
        chpl_ = model.chosenPlayers
        chpl =
            if oppositePlayer model side == player then
                chpl_
            else
                case side of
                    Model.XSide -> { chpl_ | x = player }
                    Model.OSide -> { chpl_ | o = player }
    in
        { model
        | choosingSide = Nothing
        , chosenPlayers = chpl
        }
