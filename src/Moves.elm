module Moves exposing
    ( allowedMoves
    , slide
    , whichCanMove
    )


import String


import Board


type Move =
    Slide (Int, Int) (Int, Int)


slide : (Int, Int) -> (Int, Int) -> Move
slide from to =
    Slide from to


allowedMoves : Board.Piece -> Board.Board -> List Move
allowedMoves side board =
    []


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
