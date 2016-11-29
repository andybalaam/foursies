module Model exposing
    ( allPlayers
    , newModel
    , oppositeSide
    , Flags
    , Model
    , Side(..)
    , Player(..)
    )

type alias Flags =
    { width : Int
    , height : Int
    }


type Side = XSide | OSide


oppositeSide : Side -> Side
oppositeSide side =
    case side of
        XSide -> OSide
        OSide -> XSide


type Player =
    BlackPlayer
    | WhitePlayer
    | GreenPlayer
    | BluePlayer


allPlayers : List Player
allPlayers =
    [ BlackPlayer
    , WhitePlayer
    , GreenPlayer
    , BluePlayer
    ]


type alias Model =
    { screen :
        { width : Int
        , height : Int
        }
    , chosenPlayers :
        { x : Player -- Top
        , o : Player -- Bottom
        }
    , choosingSide : Maybe Side
    , turn : Side
    }


newModel : Flags -> Model
newModel flags =
    { screen =
        { width = flags.width
        , height = flags.height
        }
    , chosenPlayers =
        { x = BlackPlayer
        , o = WhitePlayer
        }
    , choosingSide = Nothing
    , turn = XSide
    }
