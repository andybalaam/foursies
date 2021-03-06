module View exposing
    ( boardMessage
    , boardPieces
    , boardSide
    , boardTicks
    , choosePlayersDiv
    , onTouchStart
    , view
    , wonOverlay
    )


import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


import Board
import Model
import Moves
import Msg
import PixelScale


onTouchStart msg = on "touchstart" (Json.Decode.succeed msg)


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


boardMessage : Model.Model -> List (Html.Html Msg.Msg)
boardMessage model =
    case model.message of
        Model.MessageNormal ->
            [ Html.text <|
                "Scroll down for rules. Tap or drag the pieces."
                ++ " Ticks show what you can do."
            ]

        Model.MessageMoveNotAllowed ->
            [ Html.text <|
                "You are not allowed to move to there."
                ++ " Drop the piece where there is a tick."
            ]

        Model.MessageWon side ->
            [ Html.img
                [ Html.Attributes.style
                    [ ("height", "1.2em")
                    , ("vertical-align", "middle")
                    , ("margin-left", "0.2em")
                    ]
                , Html.Attributes.src
                    <| playerImage <| Model.sidePlayer model side
                ]
                []
            , Html.text " won!  Choose \"Start again\" below."
            ]

        Model.MessageMustTake ->
            [ Html.text "You must take!" ]


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
        Just (Model.DragState dx dy dMousePos) ->
            if dx /= xpos || dy /= ypos then
                (0, 0, 0, 0)
            else
                beingDraggedOffsets model dMousePos
        Just (Model.TouchedState dx dy) ->
            if dx /= xpos || dy /= ypos then
                (0, 0, 0, 0)
            else
                (-1, -1, 0, 0)
        Nothing -> (0, 0, 0, 0)


pieceListeners : Model.Model -> Int -> Int -> List (Svg.Attribute Msg.Msg)
pieceListeners model xpos ypos =
    -- TODO: combine with "offsets" above
    case model.dragging of
        Just (Model.TouchedState dx dy) ->
            if dx == xpos || dy == ypos then
                [ onTouchStart Msg.Untouched ]
            else
                []
        default -> []


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
            (
                [ x x_, y y_, height "20", width "20" -- TODO: piecewidth
                , xlinkHref (playerImage (Model.sidePlayer model side))
                ]
                ++ (pieceListeners model xpos ypos)
            )
            []
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
        Just (Model.DragState dx dy dragMouseStart) ->
            let (draggedX, draggedY) =
                PixelScale.gridDistance model dragMouseStart model.mousePos
            in
                if (dx + draggedX, dy + draggedY) == (xpos, ypos) then
                    "images/big-tick.svg"
                else
                    "images/tick.svg"
        default -> "images/tick.svg"


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
                , onTouchStart <| Msg.Touched xpos ypos
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

            Just (Model.TouchedState xpos ypos) ->
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


actualWonMessage : Model.Model -> Model.Side -> List (Html.Html Msg.Msg)
actualWonMessage model wonSide =
    let
        w = PixelScale.backgroundWidth
    in
        [
            rect
                [ x "0"
                , y "0"
                , width  (toString w)
                , height (toString w)
                , fill "#000000"
                , opacity "0.8"
                ]
                []
            , image
                [ x (toString (w * 0.25))
                , y (toString (w * 0.14))
                , height (toString (w * 0.5))
                , width (toString (w * 0.5))
                , xlinkHref
                    <| playerImage
                    <| Model.sidePlayer model wonSide
                ]
                []
            , Svg.text_
                [ x (toString (w*0.275))
                , y (toString (w*0.84))
                , fontSize (toString (w*0.2))
                , fill "#FFFFFF"
                ]
                [ Svg.text "wins!" ]
        ]


wonOverlay : Model.Model -> List (Html.Html Msg.Msg)
wonOverlay model =
    let
        canMove = Moves.whichCanMove (Model.sidePiece model.turn) model.board
    in
        case canMove of
            Moves.Won wonPiece ->
                case Model.pieceSide wonPiece of
                    Just wonSide ->
                        actualWonMessage model wonSide
                    Nothing -> [] -- Never happens - wonPiece will be X or O
            default -> []


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
                    ++ (wonOverlay model)
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
        [ Html.div [ Html.Attributes.id "msg" ] (boardMessage model)
        , boardSvg model
        , Html.div
            [ Html.Attributes.class "butdiv"
            ]
            [ Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.value "Start again"
                , Html.Attributes.class "but"
                , Html.Events.onClick Msg.StartAgain
                ]
                []
            ]
        ]


howToPlay : Html.Html Msg.Msg
howToPlay =
    Html.div
        [ Html.Attributes.style
            [ ( "width", "23em" )
            , ( "margin", "auto" )
            ]
        ]
        [ Html.h2 [] [ Html.text "How to play" ]
        , Html.div
            [ Html.Attributes.class "instr" ]
            [ Html.img
                [ Html.Attributes.class "imgleft"
                , Html.Attributes.src "images/win.svg"
                ]
                []
            , Html.div
                [ Html.Attributes.class "paright"
                , Html.Attributes.style [ ( "padding-top", "0.6em" ) ]
                ]
                [ Html.text
                    ("To win, get a piece to the other side of the "
                    ++ "board (or capture all your opponent's pieces).")
                ]
            ]
        , Html.div
            [ Html.Attributes.class "instr" ]
            [ Html.img
                [ Html.Attributes.class "imgright"
                , Html.Attributes.src "images/move-dirs.svg"
                ]
                []
            , Html.div
                [ Html.Attributes.class "parleft"
                ]
                [ Html.text
                    ("Pieces can move 1 square in any direction, "
                    ++ "including diagonally.")
                ]
            ]
        , Html.div
            [ Html.Attributes.class "instr" ]
            [ Html.img
                [ Html.Attributes.class "imgleft"
                , Html.Attributes.src "images/jump.svg"
                ]
                []
            , Html.div
                [ Html.Attributes.class "paright"
                ]
                [ Html.text
                    ("Pieces can jump over other pieces of the "
                    ++ "same colour.")
                ]
            ]
        , Html.div
            [ Html.Attributes.class "instr" ]
            [ Html.img
                [ Html.Attributes.class "imgright"
                , Html.Attributes.src "images/capture.svg"
                ]
                []
            , Html.div
                [ Html.Attributes.class "parleft"
                ]
                [ Html.text
                    "Jumping over the opponent's piece captures it."
                ]
            ]
        , Html.div
            [ Html.Attributes.class "instr" ]
            [ Html.div
                [ Html.Attributes.style [ ( "padding-top", "0.6em" ) ]
                ]
                [ Html.text "If you can capture something, you "
                , Html.strong [] [ Html.text "must" ]
                , Html.text " capture something."
                ]
            ]
        ]


viewAddress : Html.Html Msg.Msg
viewAddress =
    Html.address
        []
        [ Html.text
            ("This game is Free Software, licensed under the GPLv3"
            ++ " licence. You can find the source code at ")
        , Html.a
            [ Html.Attributes.href
                "https://github.com/andybalaam/foursies"
            ]
            [ Html.text "https://github.com/andybalaam/foursies"
            ]
        , Html.text ". The wood board image is based on "
        , Html.a
            [ Html.Attributes.href
                ("https://openclipart.org/detail/226774/"
                ++"grain-woody-texture-seamless-pattern-02")
            ]
            [ Html.text
                ("https://openclipart.org/detail/226774/"
                ++ "grain-woody-texture-seamless-pattern-02")
            ]
            , Html.text " by yamachem"
        ]


view : Model.Model -> Html.Html Msg.Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ text "Foursies" ]
        , Html.p [] [ text "A deceptively simple two-player board game" ]
        , choosePlayersDiv model
        , boardDiv model
        , howToPlay
        , viewAddress
        ]
