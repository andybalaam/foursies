module Update exposing (update)


import Model
import Msg


update : Msg.Msg -> Model.Model -> (Model.Model, Cmd Msg.Msg)
update msg model =
    let m =
        case msg of
            Msg.Resize w h -> updateResize w h model
            Msg.ChoosePlayer side -> { model | choosingSide = Just side }
            Msg.ChangePlayer side player -> updatePlayer model side player
            Msg.MouseMove pos -> { model | mousePos = pos }
            Msg.DragStart xpos ypos -> updateDragStart xpos ypos model
    in
        (m, Cmd.none)


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
