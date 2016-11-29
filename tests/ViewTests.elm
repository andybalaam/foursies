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
                ]
            ]
        ]


readyToStart : () -> Expect.Expectation
readyToStart =
    equalHtml
        rtsHtml
        (View.view <| Model.newModel {height=10, width=10})


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
