module Board exposing
    ( Board
    , Piece(OPiece, XPiece)
    , Row
    , noPiece
    , offBoard
    , oPiece
    , parse
    , pieceAt
    , positions
    , strings
    , toStrings
    , xPiece
    )


import String


type Piece = NoPiece | OPiece | XPiece | OffBoard
type Strings = Strings String String String String


type alias Row = ( Piece, Piece, Piece, Piece )


type alias Board =
    { pieces :
        ( Row
        , Row
        , Row
        , Row
        )
    }


strings : String -> String -> String -> String -> Strings
strings line1 line2 line3 line4 =
    Strings line1 line2 line3 line4


noPiece : Piece
noPiece = NoPiece


oPiece : Piece
oPiece = OPiece


xPiece : Piece
xPiece = XPiece


offBoard : Piece
offBoard = OffBoard


positions : List (Int, Int)
positions =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (0, 1)
    , (1, 1)
    , (2, 1)
    , (3, 1)
    , (0, 2)
    , (1, 2)
    , (2, 2)
    , (3, 2)
    , (0, 3)
    , (1, 3)
    , (2, 3)
    , (3, 3)
    ]


pieceAt : (Int, Int) -> Board -> Piece
pieceAt (x, y) board =
    let
        (row0, row1, row2, row3) = board.pieces
    in
        case y of
            0 -> pieceInRow x row0
            1 -> pieceInRow x row1
            2 -> pieceInRow x row2
            3 -> pieceInRow x row3
            default -> offBoard


pieceInRow : Int -> Row -> Piece
pieceInRow x row =
    let
        (p0, p1, p2, p3) = row
    in
        case x of
            0 -> p0
            1 -> p1
            2 -> p2
            3 -> p3
            default -> offBoard



parse : Strings -> Result String Board
parse strs =
    case strs of
        Strings line0 line1 line2 line3 ->
            let
                parsed =
                    ( parseRow line0
                    , parseRow line1
                    , parseRow line2
                    , parseRow line3
                    )
            in
                case parsed of
                    (Err e, _, _, _) -> Err (e ++ " (line 1)")
                    (_, Err e, _, _) -> Err (e ++ " (line 2)")
                    (_, _, Err e, _) -> Err (e ++ " (line 3)")
                    (_, _, _, Err e) -> Err (e ++ " (line 4)")
                    (Ok r0, Ok r1, Ok r2, Ok r3) ->
                        Ok
                            { pieces =
                                ( r0
                                , r1
                                , r2
                                , r3
                                )
                            }


parseRow : String -> Result String (Piece, Piece, Piece, Piece)
parseRow line =
    case (String.toList line) of
        c0 :: c1 :: c2 :: c3 :: [] ->
            let ps = (parseChar c0, parseChar c1, parseChar c2, parseChar c3)
            in
                case ps of
                    (Err e, _, _, _) -> Err (e ++ " (char 1)")
                    (_, Err e, _, _) -> Err (e ++ " (char 2)")
                    (_, _, Err e, _) -> Err (e ++ " (char 3)")
                    (_, _, _, Err e) -> Err (e ++ " (char 3)")
                    (Ok p1, Ok p2, Ok p3, Ok p4) ->
                        Ok (p1, p2, p3, p4)
        default ->
            Err "Line does not contain 4 characters"


parseChar : Char -> Result String Piece
parseChar c =
    case c of
        '.' -> Ok noPiece
        'O' -> Ok oPiece
        'X' -> Ok xPiece
        default -> Err
            ( "Invalid character " ++ (toString c)
                ++ " - expected '.', 'X', or 'O'"
            )


toStrings : Board -> Strings
toStrings board =
    let
        (row0, row1, row2, row3) = board.pieces
    in
        Strings
            (rowToString row0)
            (rowToString row1)
            (rowToString row2)
            (rowToString row3)


rowToString : Row -> String
rowToString row =
    let
        (piece0, piece1, piece2, piece3) = row
    in
        String.fromList
            <| pieceToChar piece0
            :: pieceToChar piece1
            :: pieceToChar piece2
            :: pieceToChar piece3
            :: []


pieceToChar : Piece -> Char
pieceToChar piece =
    case piece of
        NoPiece -> '.'
        OPiece -> 'O'
        XPiece -> 'X'
        OffBoard -> '#'
