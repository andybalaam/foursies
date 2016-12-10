module ViewTests exposing (all)

import Test exposing (describe,test,Test)
import Expect

import Html
import Html.Attributes
import Html.Events
import Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Board
import Model
import Msg
import View


all : Test
all =
    describe "Tests of the view code"
        [ test "Ready to start" readyToStart
        , test "Ticks on moveable pieces" ticksOnMoveablePieces
        , test "Choosing X player shows choices" choosingXPlayerShowsChoice
        , test "Choosing O player shows choices" choosingOPlayerShowsChoice
        , test "Start dragging piece is offset" startDraggingPieceOffset
        , test "Continue dragging piece is moved" continueDraggingPieceMoved
        , test "Ticks where piece can land" ticksWherePieceCanLand
        , test "Big ticks where piece will land" bigTickWherePieceWillLand
        ]


equalHtml : Html.Html Msg.Msg -> Html.Html Msg.Msg -> (() -> Expect.Expectation)
equalHtml exp act =
    \() -> Expect.equal (toString exp) (toString act)


filterAtt : String -> Svg.Attribute Msg.Msg
filterAtt =
    Svg.Attributes.filter


rtsHtml : Html.Html Msg.Msg
rtsHtml =
    Html.div
        []
        [ Html.h1 [] [ text "Foursies" ]
        , Html.p [] [ text "A deceptively simple two-player board game" ]
        , Html.div
            [ Html.Attributes.class "chooseplayers" ]
            [ text "Choose players:"
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "player"
                , Html.Attributes.style
                    [ ( "background-image" , "url('images/piece-black.svg')")
                    ]
                , Html.Events.onClick <| Msg.ChoosePlayer Model.XSide
                ]
                []
            , Html.span
                [ Html.Attributes.class "choose-player"
                , Html.Attributes.style [ ("visibility", "hidden") ]
                ]
                [ Html.input
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.class "chplayer"
                    , Html.Attributes.style
                        [
                            ( "background-image"
                            , "url('images/piece-black.svg')"
                            )
                        ]
                    , Html.Events.onClick <|
                        Msg.ChangePlayer Model.XSide Model.BlackPlayer
                    ]
                    []
                , Html.br [] []
                , Html.input
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.class "chplayer"
                    , Html.Attributes.style
                        [
                            ( "background-image"
                            , "url('images/piece-green.svg')"
                            )
                        ]
                    , Html.Events.onClick <|
                        Msg.ChangePlayer Model.XSide Model.GreenPlayer
                    ]
                    []
                , Html.br [] []
                , Html.input
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.class "chplayer"
                    , Html.Attributes.style
                        [
                            ( "background-image"
                            , "url('images/piece-blue.svg')"
                            )
                        ]
                    , Html.Events.onClick <|
                        Msg.ChangePlayer Model.XSide Model.BluePlayer
                    ]
                    []
                , Html.br [] []
                ]
            , text "vs."
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "player"
                , Html.Attributes.style
                    [ ( "background-image" , "url('images/piece-white.svg')")
                    ]
                , Html.Events.onClick <| Msg.ChoosePlayer Model.OSide
                ]
                []
            , Html.span
                [ Html.Attributes.class "choose-player"
                , Html.Attributes.style [ ("visibility", "hidden") ]
                ]
                [ Html.input
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.class "chplayer"
                    , Html.Attributes.style
                        [
                            ( "background-image"
                            , "url('images/piece-white.svg')"
                            )
                        ]
                    , Html.Events.onClick <|
                        Msg.ChangePlayer Model.OSide Model.BlackPlayer
                    ]
                    []
                , Html.br [] []
                , Html.input
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.class "chplayer"
                    , Html.Attributes.style
                        [
                            ( "background-image"
                            , "url('images/piece-green.svg')"
                            )
                        ]
                    , Html.Events.onClick <|
                        Msg.ChangePlayer Model.OSide Model.GreenPlayer
                    ]
                    []
                , Html.br [] []
                , Html.input
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.class "chplayer"
                    , Html.Attributes.style
                        [
                            ( "background-image"
                            , "url('images/piece-blue.svg')"
                            )
                        ]
                    , Html.Events.onClick <|
                        Msg.ChangePlayer Model.OSide Model.BluePlayer
                    ]
                    []
                , Html.br [] []
                ]
            ]
        , Html.div
            [ Html.Attributes.style
                [ ("width", "360px")
                , ("margin", "1em auto")
                ]
            ]
            [ Html.div
                [ Html.Attributes.id "msg" ]
                [ Html.text <|
                    "Drag the pieces to move."
                    ++ " Ticks tell you what you can move."
                , Html.text " To play:"
                , Html.img
                    [ Html.Attributes.style
                        [ ("height", "1.2em")
                        , ("vertical-align", "bottom")
                        ]
                    , Html.Attributes.src "images/piece-black.svg"
                    ]
                    []
                ]
            , svg
                [ width "360"
                , height "360"
                ]
                [ g
                    [ transform "scale(1.8,1.8)" ]
                    [ image
                        [ xlinkHref "images/board.svg"
                        , x "0"
                        , y "0"
                        , width "200"
                        , height "200"
                        ]
                        []
                    , line
                        [ x1 "5", y1 "5", x2 "195", y2 "5"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "5", y1 "52.5", x2 "195", y2 "52.5"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "5", y1 "100", x2 "195", y2 "100"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "5", y1 "147.5", x2 "195", y2 "147.5"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "5", y1 "195", x2 "195", y2 "195"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "5", y1 "5", x2 "5", y2 "195"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "52.5", y1 "5", x2 "52.5", y2 "195"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "100", y1 "5", x2 "100", y2 "195"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "147.5", y1 "5", x2 "147.5", y2 "195"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , line
                        [ x1 "195", y1 "5", x2 "195", y2 "195"
                        , strokeWidth "3", stroke "black"
                        , strokeLinecap "square", opacity "0.4" ] []
                    , g
                        [ transform "scale(2.198, 2.198)" ]
                        [ Svg.filter
                            [ id "blur" ]
                            [ feGaussianBlur
                                [ stdDeviation "0.5" ]
                                []
                            ]
                        , circle
                            [ cx "14.5", cy "14.5", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "3.1", y "3.1", height "20", width "20"
                            , xlinkHref "images/piece-black.svg"
                            ] []
                        , circle
                            [ cx "36.1", cy "14.5", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "24.700000000000003", y "3.1", height "20", width "20"
                            , xlinkHref "images/piece-black.svg" ] []
                        , circle
                            [ cx "57.7", cy "14.5", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "46.300000000000004", y "3.1", height "20", width "20"
                            , xlinkHref "images/piece-black.svg" ] []
                        , circle
                            [ cx "79.30000000000001", cy "14.5", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "67.9", y "3.1", height "20", width "20"
                            , xlinkHref "images/piece-black.svg" ] []
                        , circle
                            [ cx "14.5", cy "79.30000000000001", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "3.1", y "67.9", height "20", width "20"
                            , xlinkHref "images/piece-white.svg" ] []
                        , circle
                            [ cx "36.1", cy "79.30000000000001", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "24.700000000000003", y "67.9", height "20", width "20"
                            , xlinkHref "images/piece-white.svg" ] []
                        , circle
                            [ cx "57.7", cy "79.30000000000001", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "46.300000000000004", y "67.9", height "20", width "20"
                            , xlinkHref "images/piece-white.svg" ] []
                        , circle
                            [ cx "79.30000000000001", cy "79.30000000000001", r "10", fill "black"
                            , opacity "0.6", filterAtt "url(#blur)" ] []
                        , image
                            [ x "67.9", y "67.9", height "20", width "20"
                            , xlinkHref "images/piece-white.svg" ] []
                        , image
                            [ x "3.1", y "3.1", height "20", width "20"
                            , xlinkHref "images/tick.svg"
                            , View.onTouchStart <| Msg.Touched 0 0
                            , onMouseDown <| Msg.DragStart 0 0
                            ] []
                        , image
                            [ x "24.700000000000003", y "3.1", height "20", width "20"
                            , xlinkHref "images/tick.svg"
                            , View.onTouchStart <| Msg.Touched 1 0
                            , onMouseDown <| Msg.DragStart 1 0
                            ] []
                        , image
                            [ x "46.300000000000004", y "3.1", height "20", width "20"
                            , xlinkHref "images/tick.svg"
                            , View.onTouchStart <| Msg.Touched 2 0
                            , onMouseDown <| Msg.DragStart 2 0
                            ] []
                        , image
                            [ x "67.9", y "3.1", height "20", width "20"
                            , xlinkHref "images/tick.svg"
                            , View.onTouchStart <| Msg.Touched 3 0
                            , onMouseDown <| Msg.DragStart 3 0
                            ] []
                        ]
                    ]
                ]
            ]
        ]


readyToStart : () -> Expect.Expectation
readyToStart =
    equalHtml
        rtsHtml
        (View.view <| Model.newModel {height=400, width=400})


ticksOnPlaces : Board.Board -> Expect.Expectation
ticksOnPlaces board =
    let
        model_ = Model.newModel {height=100, width=100}
        model = { model_ | board = board, turn = Model.OSide }
    in
        Expect.equal
            ( toString
                [ image
                    [ x "24.700000000000003", y "46.300000000000004"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 3 1
                    , onMouseDown <| Msg.DragStart 3 1
                    ] []
                , image
                    [ x "67.9", y "24.700000000000003"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 1 2
                    , onMouseDown <| Msg.DragStart 1 2
                    ] []
                ]
            )
            (toString (View.boardTicks model))


ticksOnMoveablePieces : () -> Expect.Expectation
ticksOnMoveablePieces =
    let
        board = Board.parse <| Board.strings
            ".X.."
            "...O"
            ".O.."
            "...."
    in
        \() ->
            case board of
                Err e -> Expect.fail e
                Ok b ->
                    ticksOnPlaces b


chooseXHtml : Html.Html Msg.Msg
chooseXHtml =
    Html.div
        [ Html.Attributes.class "chooseplayers" ]
        [ text "Choose players:"
        , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "player"
            , Html.Attributes.style
                [ ( "background-image" , "url('images/piece-green.svg')")
                ]
            , Html.Events.onClick <| Msg.ChoosePlayer Model.XSide
            ]
            []
        , Html.span
            [ Html.Attributes.class "choose-player"
            , Html.Attributes.style [ ("visibility", "visible") ]
            ]
            [ Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-black.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.XSide Model.BlackPlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-green.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.XSide Model.GreenPlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-blue.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.XSide Model.BluePlayer
                ]
                []
            , Html.br [] []
            ]
        , text "vs."
        , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "player"
            , Html.Attributes.style
                [ ( "background-image" , "url('images/piece-white.svg')")
                ]
            , Html.Events.onClick <| Msg.ChoosePlayer Model.OSide
            ]
            []
        , Html.span
            [ Html.Attributes.class "choose-player"
            , Html.Attributes.style [ ("visibility", "hidden") ]
            ]
            [ Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-black.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.OSide Model.BlackPlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-white.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.OSide Model.WhitePlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-blue.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.OSide Model.BluePlayer
                ]
                []
            , Html.br [] []
            ]
        ]


choosingXPlayerShowsChoice : () -> Expect.Expectation
choosingXPlayerShowsChoice =
    let
        model_ = Model.newModel {height=10, width=10}
        chpl = model_.chosenPlayers
        model =
            { model_
            | chosenPlayers = {chpl | x = Model.GreenPlayer}
            , choosingSide = Just Model.XSide
            }
    in
        equalHtml
            chooseXHtml
            (View.choosePlayersDiv model)


chooseOHtml : Html.Html Msg.Msg
chooseOHtml =
    Html.div
        [ Html.Attributes.class "chooseplayers" ]
        [ text "Choose players:"
        , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "player"
            , Html.Attributes.style
                [ ( "background-image" , "url('images/piece-white.svg')")
                ]
            , Html.Events.onClick <| Msg.ChoosePlayer Model.XSide
            ]
            []
        , Html.span
            [ Html.Attributes.class "choose-player"
            , Html.Attributes.style [ ("visibility", "hidden") ]
            ]
            [ Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-black.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.XSide Model.BlackPlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-white.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.XSide Model.WhitePlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-green.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.XSide Model.GreenPlayer
                ]
                []
            , Html.br [] []
            ]
        , text "vs."
        , Html.input
            [ Html.Attributes.type_ "button"
            , Html.Attributes.class "player"
            , Html.Attributes.style
                [ ( "background-image" , "url('images/piece-blue.svg')")
                ]
            , Html.Events.onClick <| Msg.ChoosePlayer Model.OSide
            ]
            []
        , Html.span
            [ Html.Attributes.class "choose-player"
            , Html.Attributes.style [ ("visibility", "visible") ]
            ]
            [ Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-black.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.OSide Model.BlackPlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-green.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.OSide Model.GreenPlayer
                ]
                []
            , Html.br [] []
            , Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.class "chplayer"
                , Html.Attributes.style
                    [
                        ( "background-image"
                        , "url('images/piece-blue.svg')"
                        )
                    ]
                , Html.Events.onClick <|
                    Msg.ChangePlayer Model.OSide Model.BluePlayer
                ]
                []
            , Html.br [] []
            ]
        ]


choosingOPlayerShowsChoice : () -> Expect.Expectation
choosingOPlayerShowsChoice =
    let
        model_ = Model.newModel {height=10, width=10}
        chpl = model_.chosenPlayers
        model =
            { model_
            | chosenPlayers =
                { chpl
                | x = Model.WhitePlayer
                , o = Model.BluePlayer
                }
            , choosingSide = Just Model.OSide
            }
    in
        equalHtml
            chooseOHtml
            (View.choosePlayersDiv model)


pieceOffsetHtml : List (Html.Html Msg.Msg)
pieceOffsetHtml =
    [ circle
        [ cx "36.1", cy "14.5", r "10", fill "black"
        , opacity "0.6", filterAtt "url(#blur)" ] []
    , image
        [ x "23.700000000000003", y "2.1", height "20", width "20"
        , xlinkHref "images/piece-black.svg"
        , onMouseDown <| Msg.DragStart 1 0
        ] []
    ]


startDraggingPieceOffset : () -> Expect.Expectation
startDraggingPieceOffset =
    let
        model_ = Model.newModel {height=100, width=100}
        model =
            { model_
            | dragging = Just <| Model.DragState 1 0 (Mouse.Position 34 36)
            , mousePos = Mouse.Position 34 36  -- Mouse not moved
            }
    in
        \() -> Expect.equal
            pieceOffsetHtml
            (View.boardSide model Model.XSide (1, 0))


pieceMovedHtml : List (Html.Html Msg.Msg)
pieceMovedHtml =
    [ circle
        [ cx "41.155100596501875", cy "34.72040238600748", r "10", fill "black"
        , opacity "0.6", filterAtt "url(#blur)" ] []
    , image
        [ x "28.755100596501872", y "22.320402386007483"
        , height "20", width "20"
        , xlinkHref "images/piece-black.svg"
        , onMouseDown <| Msg.DragStart 1 0
        ] []
    ]


continueDraggingPieceMoved : () -> Expect.Expectation
continueDraggingPieceMoved =
    let
        model_ = Model.newModel {height=100, width=100}
        model =
            { model_
            | dragging = Just <| Model.DragState 1 0 (Mouse.Position 34 36)
            , mousePos = Mouse.Position 39 56  -- Mouse moved
            }
    in
        \() -> Expect.equal
            pieceMovedHtml
            (View.boardSide model Model.XSide (1, 0))


ticksWhereLand : Board.Board -> Expect.Expectation
ticksWhereLand board =
    let
        model_ = Model.newModel {width=100, height=100}
        model =
            { model_
            | board = board
            , turn = Model.OSide
            , dragging = Just <| Model.DragState 3 1 (Mouse.Position 0 0)
            }
    in
        Expect.equal
            ( toString
                [ image
                    [ x "67.9", y "3.1"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 3 0
                    ] []
                , image
                    [ x "67.9", y "46.300000000000004"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 3 2
                    ] []
                , image
                    [ x "46.300000000000004", y "46.300000000000004"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 2 2
                    ] []
                , image
                    [ x "46.300000000000004", y "24.700000000000003"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 2 1
                    ] []
                , image
                    [ x "46.300000000000004", y "3.1"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 3 0
                    ] []
                ]
            )
            (toString (View.boardTicks model))


ticksWherePieceCanLand : () -> Expect.Expectation
ticksWherePieceCanLand =
    let
        board = Board.parse <| Board.strings
            ".X.."
            "...O"
            ".O.."
            "...."
    in
        \() ->
            case board of
                Err e -> Expect.fail e
                Ok b ->
                    ticksWhereLand b


bigTick : Board.Board -> Expect.Expectation
bigTick board =
    let
        model_ = Model.newModel {width=100, height=100}
        model =
            { model_
            | board = board
            , turn = Model.OSide
            , dragging = Just <| Model.DragState 3 1 (Mouse.Position 67 100)
            , mousePos = Mouse.Position 67 85
            }
    in
        Expect.equal
            ( toString
                [ image
                    [ x "67.9", y "3.1"
                    , height "20", width "20"
                    , xlinkHref "images/big-tick.svg"
                    , View.onTouchStart <| Msg.Touched 3 0
                    ] []
                , image
                    [ x "67.9", y "46.300000000000004"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 3 2
                    ] []
                , image
                    [ x "46.300000000000004", y "46.300000000000004"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 2 2
                    ] []
                , image
                    [ x "46.300000000000004", y "24.700000000000003"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 2 1
                    ] []
                , image
                    [ x "46.300000000000004", y "3.1"
                    , height "20", width "20"
                    , xlinkHref "images/tick.svg"
                    , View.onTouchStart <| Msg.Touched 2 1
                    ] []
                ]
            )
            (toString (View.boardTicks model))


bigTickWherePieceWillLand : () -> Expect.Expectation
bigTickWherePieceWillLand =
    let
        board = Board.parse <| Board.strings
            ".X.."
            "...O"
            ".O.."
            "...."
    in
        \() ->
            case board of
                Err e -> Expect.fail e
                Ok b ->
                    bigTick b
