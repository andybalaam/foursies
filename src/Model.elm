module Model exposing
    ( allPlayers
    , newModel
    , oppositeSide
    , pieceSide
    , sidePlayer
    , sidePiece
    , startingBoard
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


sidePiece : Side -> Board.Piece
sidePiece side =
    case side of
        XSide -> Board.XPiece
        OSide -> Board.OPiece


pieceSide : Board.Piece -> Maybe Side
pieceSide piece =
    case piece of
        Board.XPiece -> Just XSide
        Board.OPiece -> Just OSide
        default -> Nothing


type Message =
    MessageNormal
    | MessageMoveNotAllowed
    | MessageWon Side
    | MessageMustTake


type Drag =
    DragState Int Int Mouse.Position
    | TouchedState Int Int


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
    , turn : Side            -- TODO: combine board and side
    , board : Board.Board    --       together before making undo/redo
    , dragging : Maybe Drag
    , mousePos : Mouse.Position
    }


startingBoard : Board.Board
startingBoard =
    Board.newBoard
        Board.xPiece  Board.xPiece  Board.xPiece  Board.xPiece
        Board.noPiece Board.noPiece Board.noPiece Board.noPiece
        Board.noPiece Board.noPiece Board.noPiece Board.noPiece
        Board.oPiece  Board.oPiece  Board.oPiece  Board.oPiece


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
    , board = startingBoard
    , dragging = Nothing
    , mousePos = Mouse.Position 0 0
    }
