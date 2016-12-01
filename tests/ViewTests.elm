module ViewTests exposing (all)

import Test exposing (describe,test,Test)
import Expect

import Html
import Html.Attributes
import Html.Events
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
        , test "Choosing X player shows choices" choosingXPlayerShowsChoice
        , test "Choosing O player shows choices" choosingOPlayerShowsChoice
        ]


equalHtml : Html.Html Msg.Msg -> Html.Html Msg.Msg -> (() -> Expect.Expectation)
equalHtml exp act =
    \() -> Expect.equal (toString exp) (toString act)


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
                [ Html.text "Drag the pieces to move. To play: "
                , Html.img
                    [ Html.Attributes.style
                        [ ("height", "1.2em")
                        , ("vertical-align", "bottom")
                        ]
                    , Html.Attributes.src "images/piece-black.svg"
                    ]
                    []
                , Html.br [] []
                , Html.text "Ticks tell you what you can move."
                ]
            ]
        ]


readyToStart : () -> Expect.Expectation
readyToStart =
    equalHtml
        rtsHtml
        (View.view <| Model.newModel {height=400, width=400})


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
