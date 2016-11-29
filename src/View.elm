module View exposing (view)


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


sidePlayer : Model.Model -> Model.Side -> Model.Player
sidePlayer model side =
    case side of
        Model.XSide -> model.chosenPlayers.x
        Model.OSide -> model.chosenPlayers.o


playerStyle : Model.Player -> List ( String, String )
playerStyle player =
    [ ("background-image", "url('" ++ (playerImage player) ++ "')") ]


playerChoiceInput : Model.Side -> Model.Player -> Html.Html Msg.Msg
playerChoiceInput side player =
    Html.input
        [ Html.Attributes.type_ "button"
        , Html.Attributes.class "chplayer"
        , Html.Attributes.style <| playerStyle player
        , Html.Events.onClick <|
            Msg.ChangePlayer side player
        ]
        []


playerChoiceSpan : Model.Model -> Model.Side -> Html.Html Msg.Msg
playerChoiceSpan model side =
    let
        oppSide = Model.oppositeSide side
    in
        Html.span
            [ Html.Attributes.class "choose-player"
            , Html.Attributes.style [ ("visibility", "hidden") ]
            ]
            (
                List.map (playerChoiceInput side) <|
                    List.filter
                        (\p -> p /= sidePlayer model oppSide)
                        Model.allPlayers
            )


choosePlayerInput : Model.Model -> Model.Side -> Html.Html Msg.Msg
choosePlayerInput model side =
    Html.input
        [ Html.Attributes.type_ "button"
        , Html.Attributes.class "player"
        , Html.Attributes.style <| playerStyle <| sidePlayer model side
        , Html.Events.onClick <| Msg.ChoosePlayer side
        ]
        []


view : Model.Model -> Html.Html Msg.Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ text "Foursies" ]
        , Html.p [] [ text "A deceptively simple two-player board game" ]
        , Html.div
            [ Html.Attributes.class "chooseplayers" ]
            [ text "Choose players:"
            , choosePlayerInput model Model.XSide
            , playerChoiceSpan model Model.XSide
            , text "vs."
            , choosePlayerInput model Model.OSide
            , playerChoiceSpan model Model.OSide
            ]
            --svg
            --[ width  <| toString model.screen.width
            --, height <| toString model.screen.height
            --]
            --[
            --]
        ]
