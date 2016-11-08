module Moves exposing
    ( whichCanMove
    )


import String


import Board


whichCanMove : Board.Piece -> Board.Board -> List (Int, Int)
whichCanMove piece board =
    let
        (row0, row1, row2, row3) = board.pieces
    in
        (  whichCanMoveInRow piece 0 row0
        ++ whichCanMoveInRow piece 1 row1
        ++ whichCanMoveInRow piece 2 row2
        ++ whichCanMoveInRow piece 3 row3
        )

whichCanMoveInRow : Board.Piece -> Int -> Board.Row -> List (Int, Int)
whichCanMoveInRow piece y row =
    let
        (s0, s1, s2, s3) = row
    in
        (  whichCanMoveInSquare piece 0 y s0
        ++ whichCanMoveInSquare piece 1 y s1
        ++ whichCanMoveInSquare piece 2 y s2
        ++ whichCanMoveInSquare piece 3 y s3
        )


whichCanMoveInSquare :
    Board.Piece -> Int -> Int -> Board.Piece -> List (Int, Int)
whichCanMoveInSquare piece x y thisPiece =
    if piece == thisPiece then [(x, y)] else []
