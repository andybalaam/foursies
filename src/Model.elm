module Model exposing
    ( allPlayers
    , newModel
    , oppositeSide
    , sidePlayer
    , Drag(..)
    , Flags
    , Message(..)
    , Model
    , Side(..)
    , Player(..)
    )


import Mouse


import Board


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
    | MessageDragging Int Int


type Drag =
    DragState Int Int Int Int


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
    , board : Board.Board
    , dragging : Maybe Drag
    , mousePos : Mouse.Position
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
    , board = Board.newBoard
        Board.xPiece  Board.xPiece  Board.xPiece  Board.xPiece
        Board.noPiece Board.noPiece Board.noPiece Board.noPiece
        Board.noPiece Board.noPiece Board.noPiece Board.noPiece
        Board.oPiece  Board.oPiece  Board.oPiece  Board.oPiece
    , dragging = Nothing
    , mousePos = Mouse.Position 0 0
    }
