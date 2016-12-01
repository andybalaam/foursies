module Model exposing
    ( allPlayers
    , newModel
    , oppositeSide
    , sidePlayer
    , Flags
    , Message(..)
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


sidePlayer : Model -> Side -> Player
sidePlayer model side =
    case side of
        XSide -> model.chosenPlayers.x
        OSide -> model.chosenPlayers.o


type Message =
    MessageNormal


type alias Model =
    { screen :
        { width : Int
        , height : Int
        }
    , message : Message
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
    , message = MessageNormal
    , chosenPlayers =
        { x = BlackPlayer
        , o = WhitePlayer
        }
    , choosingSide = Nothing
    , turn = XSide
    }
