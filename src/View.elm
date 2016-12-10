module View exposing (boardSide, boardTicks, choosePlayersDiv, view)


import Html
import Html.Attributes
import Html.Events
import Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


import Board
import Model
import Moves
import Msg
import PixelScale


playerImage : Model.Player -> String
playerImage player =
    case player of
        Model.BlackPlayer -> "images/piece-black.svg"
        Model.WhitePlayer -> "images/piece-white.svg"
        Model.GreenPlayer -> "images/piece-green.svg"
        Model.BluePlayer -> "images/piece-blue.svg"


playerStyle : Model.Player -> List ( String, String )
playerStyle player =
    [ ("background-image", "url('" ++ (playerImage player) ++ "')") ]


playerChoiceInput : Model.Side -> Model.Player -> List (Html.Html Msg.Msg)
playerChoiceInput side player =
    [ Html.input
        [ Html.Attributes.type_ "button"
        , Html.Attributes.class "chplayer"
        , Html.Attributes.style <| playerStyle player
        , Html.Events.onClick <|
            Msg.ChangePlayer side player
        ]
        []
    , Html.br [] []
    ]


playerChoiceVisibility : Model.Model -> Model.Side -> String
playerChoiceVisibility model side =
    if model.choosingSide == Just side then
        "visible"
    else
        "hidden"


playerChoiceSpan : Model.Model -> Model.Side -> Html.Html Msg.Msg
playerChoiceSpan model side =
    let
        oppSide = Model.oppositeSide side
    in
        Html.span
            [ Html.Attributes.class "choose-player"
            , Html.Attributes.style
                [ ("visibility", playerChoiceVisibility model side ) ]
            ]
            (
                List.concat
                    ( List.map (playerChoiceInput side) <|
                        List.filter
                            (\p -> p /= Model.sidePlayer model oppSide)
                            Model.allPlayers
                    )
            )


choosePlayerInput : Model.Model -> Model.Side -> Html.Html Msg.Msg
choosePlayerInput model side =
    Html.input
        [ Html.Attributes.type_ "button"
        , Html.Attributes.class "player"
        , Html.Attributes.style <| playerStyle <| Model.sidePlayer model side
        , Html.Events.onClick <| Msg.ChoosePlayer side
        ]
        []


choosePlayersDiv : Model.Model -> Html.Html Msg.Msg
choosePlayersDiv model =
    Html.div
        [ Html.Attributes.class "chooseplayers" ]
        [ text "Choose players:"
        , choosePlayerInput model Model.XSide
        , playerChoiceSpan model Model.XSide
        , text "vs."
        , choosePlayerInput model Model.OSide
        , playerChoiceSpan model Model.OSide
        ]


toPlay : Model.Model -> List (Html.Html Msg.Msg)
toPlay model =
    [ Html.text " To play:"
    , Html.img
        [ Html.Attributes.style
            [ ("height", "1.2em")
            , ("vertical-align", "bottom")
            ]
        , Html.Attributes.src
            <| playerImage <| Model.sidePlayer model model.turn
        ]
        []
    ]


boardMessage : Model.Model -> List (Html.Html Msg.Msg)
boardMessage model =
    let mainMsg =
        case model.message of
            Model.MessageNormal ->
                [ Html.text <|
                    "Drag the pieces to move. "
                    ++ "Ticks tell you what you can move."
                ]
            Model.MessageMoveNotAllowed ->
                [ Html.text <|
                    "You are not allowed to move to there."
                    ++ "Drop the piece where there is a tick."
                ]
    in
        mainMsg ++ (toPlay model)


boardLine : String -> String -> String -> String -> Html.Html Msg.Msg
boardLine x1_ y1_ x2_ y2_ =
    line
        [ x1 x1_, y1 y1_, x2 x2_, y2 y2_
        , strokeWidth "3", stroke "black"
        , strokeLinecap "square", opacity "0.4" ] []


lineOffset : Int -> String
lineOffset i =
    toString <| 5 + (47.5 * (toFloat i))


boardLineX : Int -> Html.Html Msg.Msg
boardLineX i =
    let
        x = lineOffset i
    in
        boardLine x "5" x "195"


boardLineY : Int -> Html.Html Msg.Msg
boardLineY i =
    let
        y = lineOffset i
    in
        boardLine "5" y "195" y


boardLines : List (Html.Html Msg.Msg)
boardLines =
    (  List.map boardLineY (List.range 0 4)
    ++ (List.map boardLineX (List.range 0 4))
    )


filterAtt : String -> Svg.Attribute Msg.Msg
filterAtt =
    Svg.Attributes.filter


beingDraggedOffsets : Model.Model -> Mouse.Position
    -> (Float, Float, Float, Float)
beingDraggedOffsets model dMousePos =
    let
        offsetX = -1 -- How much a piece floats off the board when dragged
        offsetY = -1
        (movedX, movedY) =
            PixelScale.boardDistance model dMousePos model.mousePos
    in
        (offsetX + movedX, offsetY + movedY, movedX, movedY)


-- Given a model and the x and y position of the piece, return its
-- scaled offset and that of its shadow, based on whether it is being
-- dragged, and if so, where the mouse has moved.
offsets : Model.Model -> Int -> Int -> (Float, Float, Float, Float)
offsets model xpos ypos =
    case model.dragging of
        Nothing -> (0, 0, 0, 0)
        Just (Model.DragState dx dy dMousePos) ->
            if dx /= xpos || dy /= ypos then
                (0, 0, 0, 0)
            else
                beingDraggedOffsets model dMousePos


boardSide : Model.Model -> Model.Side -> (Int, Int) -> List (Html.Html Msg.Msg)
boardSide model side (xpos, ypos) =
    let
        (pieceX, pieceY, shadowX, shadowY) = offsets model xpos ypos
        scale = \start val -> start + (21.6 * (toFloat val))
        cx_ = toString <| shadowX + (scale 14.5 xpos) -- TODO: pieceWidth
        cy_ = toString <| shadowY + (scale 14.5 ypos)
        x_  = toString <| pieceX  + (scale  3.1 xpos)
        y_  = toString <| pieceY  + (scale  3.1 ypos)
        -- TODO: blur shadow more when dragged
        -- TODO: move shadow down-right
        -- TODO: draw the image later, so it's on top
    in
        [ circle
            [ cx cx_, cy cy_, r "10", fill "black"
            , opacity "0.6", filterAtt "url(#blur)" ] []
        , image
            [ x x_, y y_, height "20", width "20" -- TODO: piecewidth
            , xlinkHref (playerImage (Model.sidePlayer model side))
            ] []
        ]


boardPiece : Model.Model -> (Int, Int) -> List (Html.Html Msg.Msg)
boardPiece model pos =
    let
        piece = Board.pieceAt pos model.board
    in
        case piece of
            Board.NoPiece -> []
            Board.OffBoard -> []
            Board.XPiece -> boardSide model Model.XSide pos
            Board.OPiece -> boardSide model Model.OSide pos


tickImage : Model.Model -> Int -> Int -> String
tickImage model xpos ypos =
    case model.dragging of
        Nothing -> "images/tick.svg"
        Just (Model.DragState dx dy dragMouseStart) ->
            let (draggedX, draggedY) =
                PixelScale.gridDistance model dragMouseStart model.mousePos
            in
                if (dx + draggedX, dy + draggedY) == (xpos, ypos) then
                    "images/big-tick.svg"
                else
                    "images/tick.svg"


boardTick : Model.Model -> Bool -> (Int, Int) -> Html.Html Msg.Msg
boardTick model handleMouseDown (xpos, ypos) =
    let
        scale = \start val -> start + (21.6 * (toFloat val))
        x_  = toString <| (scale  3.1 xpos)
        y_  = toString <| (scale  3.1 ypos)
    in
        image
            (
                [ x x_, y y_, height "20", width "20"
                , xlinkHref <| tickImage model xpos ypos
                -- TODO , Svg.Attributes.style "cursor: move"
                ] ++
                    if handleMouseDown then
                        [ onMouseDown <| Msg.DragStart xpos ypos ]
                    else
                        []
            )
            []


boardTicks : Model.Model -> List (Html.Html Msg.Msg)
boardTicks model =
    let
        piece = Model.sidePiece model.turn
    in
        case model.dragging of

            Nothing ->
                case Moves.whichCanMove piece model.board of
                    Moves.Won wonPiece -> [] -- TODO: say you won
                    Moves.CanMovePositions posList ->
                        List.map (boardTick model True) posList

            Just (Model.DragState xpos ypos _) ->
                List.map
                    (boardTick model False)
                    (Moves.allowedEnds piece model.board (xpos, ypos))


boardPieces : Model.Model -> Html.Html Msg.Msg
boardPieces model =
    g
        [ transform <|
            "scale(" ++ (toString PixelScale.piecesScale)
            ++ ", "  ++ (toString PixelScale.piecesScale) ++ ")"
        ]
        (
            [ Svg.filter
                [ id "blur" ]
                [ feGaussianBlur
                    [ stdDeviation "0.5" ]
                    []
                ]
            ]
            ++ ( List.concat <| List.map (boardPiece model) Board.positions )
            ++ ( boardTicks model )
        )


boardSvg : Model.Model -> Html.Html Msg.Msg
boardSvg model =
    let
        scale = toString <| PixelScale.boardScale model
        wstr = toString <| PixelScale.boardWidth model
    in
        svg
            [ width wstr
            , height wstr
            ]
            [ g
                [ transform <| "scale(" ++ scale ++ "," ++ scale ++ ")" ]
                (
                    [ image
                        [ xlinkHref "images/board.svg"
                        , x "0"
                        , y "0"
                        , width  (toString PixelScale.backgroundWidth)
                        , height (toString PixelScale.backgroundWidth)
                        ]
                        []
                    ]
                    ++ boardLines
                    ++ [ boardPieces model ]
                )
            ]


boardDiv : Model.Model -> Html.Html Msg.Msg
boardDiv model =
    Html.div
        [ Html.Attributes.style
            [ ("width", (toString (PixelScale.boardWidth model)) ++ "px")
            , ("margin", "1em auto")
            ]
        ]
        [ Html.div
            [ Html.Attributes.id "msg" ]
            (boardMessage model)
        , boardSvg model
        ]


view : Model.Model -> Html.Html Msg.Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ text "Foursies" ]
        , Html.p [] [ text "A deceptively simple two-player board game" ]
        , choosePlayersDiv model
        , boardDiv model
        ]
