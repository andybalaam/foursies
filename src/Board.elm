module Board exposing (blackPiece, noPiece, parse, pieceAt, whitePiece)


type Piece = NoPiece | WhitePiece | BlackPiece

type alias Board =
    {}

noPiece : Piece
noPiece = NoPiece


whitePiece : Piece
whitePiece = WhitePiece


blackPiece : Piece
blackPiece = BlackPiece


pieceAt : (Int, Int) -> Board -> Piece
pieceAt (x, y) board =
    noPiece


parse : String -> String -> String -> String -> Board
parse line1 line2 line3 line4 =
    {}
