import Html.App exposing (programWithFlags)
import Window


import Model
import Msg
import View
import Update


subscriptions : Model.Model -> Sub Msg.Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\size -> Resize size.width size.height)
        ]


init : Flags -> (Model.Model, Cmd Update.Msg)
init flags =
    (Model.newModel flags, Cmd.none)


main =
   programWithFlags
     { init = Model.init
     , view = View.view
     , update = Update.update
     , subscriptions = Update.subscriptions
     }

