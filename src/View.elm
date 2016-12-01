module View exposing (choosePlayersDiv, view)


import Html
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


import Model
import Msg


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


boardWidth : Model.Model -> Int
boardWidth model =
    let
        minD = Basics.min model.screen.width model.screen.height
    in
        round <| (toFloat minD) * 0.9


boardMessage : Model.Model -> List (Html.Html Msg.Msg)
boardMessage model =
    case model.message of
        Model.MessageNormal ->
            [ Html.text "Drag the pieces to move. To play: "
            , Html.img
                [ Html.Attributes.style
                    [ ("height", "1.2em")
                    , ("vertical-align", "bottom")
                    ]
                , Html.Attributes.src
                    <| playerImage <| Model.sidePlayer model model.turn
                ]
                []
            , Html.br [] []
            , Html.text "Ticks tell you what you can move."
    ]


boardSvg : Model.Model -> Html.Html Msg.Msg
boardSvg model =
    let
        w = boardWidth model
        scale = toString <| (toFloat w) / 200
        wstr = toString w
    in
        svg
            [ width wstr
            , height wstr
            ]
            [ g
                [ transform <| "scale(" ++ scale ++ "," ++ scale ++")" ]
                [ image
                    [ xlinkHref "images/board.svg"
                    , x "0"
                    , y "0"
                    , width "200"
                    , height "200"
                    ]
                    []
                ]
            ]


boardDiv : Model.Model -> Html.Html Msg.Msg
boardDiv model =
    Html.div
        [ Html.Attributes.style
            [ ("width", (toString (boardWidth model)) ++ "px")
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
            --svg
            --[ width  <| toString model.screen.width
            --, height <| toString model.screen.height
            --]
            --[
            --]
        ]
