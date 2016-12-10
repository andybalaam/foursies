module Update exposing (MoveAllowed(..), update, dragMoveAllowed)


import Mouse


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
            Msg.Touched xpos ypos -> updateTouched xpos ypos model
    in
        (m, Cmd.none)


type MoveAllowed = YesAllowed Moves.Move | NotAllowed Model.Message


moveIsFromAndTo : (Int, Int) -> (Int, Int) -> Moves.Move -> Bool
moveIsFromAndTo fromPos toPos move =
    (Moves.to move == toPos) && (Moves.from move == fromPos)


dragMoveAllowed : Model.Model -> Int -> Int -> Mouse.Position -> MoveAllowed
dragMoveAllowed model xpos ypos startPx =
    let
        (moveX, moveY) = PixelScale.gridDistance model startPx model.mousePos
    in
        moveAllowed model xpos ypos (xpos+moveX) (ypos+moveY)


moveAllowed : Model.Model -> Int -> Int -> Int -> Int -> MoveAllowed
moveAllowed model startX startY endX endY =
    if startX == endX && startY == endY then
        NotAllowed Model.MessageNormal
    else
        let
            thisMove = List.filter
                (moveIsFromAndTo (startX, startY) (endX, endY))
                (Moves.allowedMoves
                    (Model.sidePiece model.turn) model.board)
        in
            case List.head thisMove of
                Nothing -> NotAllowed Model.MessageMoveNotAllowed
                Just move -> YesAllowed move


updateDragStop : Model.Model -> Model.Model
updateDragStop model =
    case model.dragging of
        Just (Model.DragState xpos ypos startPx) ->
            updateTryMove model (dragMoveAllowed model xpos ypos startPx)
        default -> model -- Ignore if we were not dragging


updateTryMove : Model.Model -> MoveAllowed -> Model.Model
updateTryMove model moveA =
    case moveA of
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


updateTouched : Int -> Int -> Model.Model -> Model.Model
updateTouched xpos ypos model =
    case model.dragging of
        Just (Model.TouchedState startX startY) ->
            updateTryMove model (moveAllowed model startX startY xpos ypos)
        default -> { model | dragging = Just <| Model.TouchedState xpos ypos }


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
