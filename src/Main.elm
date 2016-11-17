import Html.App exposing (programWithFlags)
import Window


import Model
import Msg
import View
import Update


subscriptions : Model.Model -> Sub Msg.Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\size -> Msg.Resize size.width size.height)
        ]


init : Model.Flags -> (Model.Model, Cmd Msg.Msg)
init flags =
    (Model.newModel flags, Cmd.none)


main =
   programWithFlags
     { init = init
     , view = View.view
     , update = Update.update
     , subscriptions = subscriptions
     }

