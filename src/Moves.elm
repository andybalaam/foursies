module Moves exposing
    ( Move(Hop, Slide, Take)
    , allowedMoves
    , hop
    , slide
    , take
    , whichCanMove
    )


import String


import Board


type Move =
    Slide (Int, Int) (Int, Int)
    | Hop (Int, Int) (Int, Int) (Int, Int)
    | Take (Int, Int) (Int, Int) (Int, Int)


slide : (Int, Int) -> (Int, Int) -> Move
slide from to =
    Slide from to


hop : (Int, Int) -> (Int, Int) -> (Int, Int) -> Move
hop from over to =
    Hop from over to


take : (Int, Int) -> (Int, Int) -> (Int, Int) -> Move
take from over to =
    Take from over to


adjust : (Int, Int) -> (Int, Int) -> (Int, Int)
adjust (pos0, pos1) (delta0, delta1) =
    (pos0 + delta0, pos1 + delta1)


allowedMoves : Board.Piece -> Board.Board -> List Move
allowedMoves side board =
    let
        ourPieces =
            List.filter
                (\pos -> Board.pieceAt pos board == side)
                Board.positions
    in
        List.concat <| List.map
            (movesForPos side board)
            ourPieces


movesForPos : Board.Piece -> Board.Board -> (Int, Int) -> List Move
movesForPos side board pos =
    List.filter
        (allowedMove side board)
        [ slide pos (adjust pos ( 0, -1))
        , slide pos (adjust pos ( 1, -1))
        , slide pos (adjust pos ( 1,  0))
        , slide pos (adjust pos ( 1,  1))
        , slide pos (adjust pos ( 0,  1))
        , slide pos (adjust pos (-1,  1))
        , slide pos (adjust pos (-1,  0))
        , slide pos (adjust pos (-1, -1))
        , hop   pos (adjust pos ( 0, -1)) (adjust pos ( 0, -2))
        , hop   pos (adjust pos ( 1, -1)) (adjust pos ( 2, -2))
        , hop   pos (adjust pos ( 1,  0)) (adjust pos ( 2,  0))
        , hop   pos (adjust pos ( 1,  1)) (adjust pos ( 2,  2))
        , hop   pos (adjust pos ( 0,  1)) (adjust pos ( 0,  2))
        , hop   pos (adjust pos (-1,  1)) (adjust pos (-2,  2))
        , hop   pos (adjust pos (-1,  0)) (adjust pos (-2,  0))
        , hop   pos (adjust pos (-1, -1)) (adjust pos (-2, -2))
        , take  pos (adjust pos ( 0, -1)) (adjust pos ( 0, -2))
        , take  pos (adjust pos ( 1, -1)) (adjust pos ( 2, -2))
        , take  pos (adjust pos ( 1,  0)) (adjust pos ( 2,  0))
        , take  pos (adjust pos ( 1,  1)) (adjust pos ( 2,  2))
        , take  pos (adjust pos ( 0,  1)) (adjust pos ( 0,  2))
        , take  pos (adjust pos (-1,  1)) (adjust pos (-2,  2))
        , take  pos (adjust pos (-1,  0)) (adjust pos (-2,  0))
        , take  pos (adjust pos (-1, -1)) (adjust pos (-2, -2))
        ]


opposite : Board.Piece -> Board.Piece
opposite side =
    case side of
        Board.XPiece -> Board.oPiece
        Board.OPiece -> Board.xPiece
        a -> a


allowedMove : Board.Piece -> Board.Board -> Move -> Bool
allowedMove side board move =
    case move of
        -- There is space to land
        Slide from to    -> Board.pieceAt to board == Board.noPiece

        -- There is space to land, and we are hopping over
        -- our own piece.
        Hop from over to ->
            (  Board.pieceAt to board == Board.noPiece
            && Board.pieceAt over board == side
            )

        -- There is space to land, and we are the opponent's
        -- piece.
        Take from over to ->
            (  Board.pieceAt to board == Board.noPiece
            && Board.pieceAt over board == opposite side
            )



whichCanMove : Board.Piece -> Board.Board -> List (Int, Int)
whichCanMove side board =
    let
        (row0, row1, row2, row3) = board.pieces
    in
        (  whichCanMoveInRow side 0 row0
        ++ whichCanMoveInRow side 1 row1
        ++ whichCanMoveInRow side 2 row2
        ++ whichCanMoveInRow side 3 row3
        )

whichCanMoveInRow : Board.Piece -> Int -> Board.Row -> List (Int, Int)
whichCanMoveInRow side y row =
    let
        (s0, s1, s2, s3) = row
    in
        (  whichCanMoveInSquare side 0 y s0
        ++ whichCanMoveInSquare side 1 y s1
        ++ whichCanMoveInSquare side 2 y s2
        ++ whichCanMoveInSquare side 3 y s3
        )


whichCanMoveInSquare :
    Board.Piece -> Int -> Int -> Board.Piece -> List (Int, Int)
whichCanMoveInSquare side x y thisPiece =
    if side == thisPiece then [(x, y)] else []
