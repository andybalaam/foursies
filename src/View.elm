module View exposing (view)


import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


import Model
import Msg


view : Model.Model -> Html Msg.Msg
view model =
    svg
    [ width  <| toString model.screen.width
    , height <| toString model.screen.height
    ]
    [
    ]
