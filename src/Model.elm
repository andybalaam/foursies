module Model exposing (newModel, Flags, Model)

type alias Flags =
    { width : Int
    , height : Int
    }


type alias Model =
    { screen :
        { width : Int
        , height : Int
        }
    }


newModel : Flags -> Model
newModel flags =
    { screen =
        { width = flags.width
        , height = flags.height
        }
    }


