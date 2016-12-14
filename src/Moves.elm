module Moves exposing
    ( Move(Hop, Slide, Take)
    , WhoCanMove(CanMovePositions, Won)
    , allowedEnds
    , allowedMoves
    , from
    , to
    , hop
    , movePiece
    , slide
    , take
    , whichCanMove
    , whoWon
    )


import Set
import String


-- TODO: consider Model.Side instead of Board.Piece everywhere
import Board


type Move =
    Slide (Int, Int) (Int, Int)
    | Hop (Int, Int) (Int, Int) (Int, Int)
    | Take (Int, Int) (Int, Int) (Int, Int)


type WhoCanMove =
    CanMovePositions (List (Int, Int))
    | Won Board.Piece



takenEquals : Move -> (Int, Int) -> Bool
takenEquals move pos =
    case taken move of
        Just takenPos -> takenPos == pos
        default       -> False


movePiecePiece : Board.Board -> (Int, Int) -> Board.Piece -> Move
    -> Board.Piece
movePiecePiece board pos piece move =
    if (from move) == pos then
        Board.noPiece
    else if to move == pos then
        (Board.pieceAt (from move) board)
    else if (takenEquals move pos) then
        Board.noPiece
    else
        piece


movePieceRow :
    Board.Board ->
    Int ->
    (Board.Piece, Board.Piece, Board.Piece, Board.Piece)
    -> Move
    -> (Board.Piece, Board.Piece, Board.Piece, Board.Piece)
movePieceRow board ypos (p0, p1, p2, p3) move =
    ( movePiecePiece board (0, ypos) p0 move
    , movePiecePiece board (1, ypos) p1 move
    , movePiecePiece board (2, ypos) p2 move
    , movePiecePiece board (3, ypos) p3 move
    )


movePieceRows :
    Board.Board ->
    (Board.Row, Board.Row, Board.Row, Board.Row)
    -> Move
    -> (Board.Row, Board.Row, Board.Row, Board.Row)
movePieceRows board (r0, r1, r2, r3) move =
    ( movePieceRow board 0 r0 move
    , movePieceRow board 1 r1 move
    , movePieceRow board 2 r2 move
    , movePieceRow board 3 r3 move
    )


movePiece : Board.Board -> Move -> Board.Board
movePiece board move =
    { pieces =
        movePieceRows board board.pieces move
    }


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


from : Move -> (Int, Int)
from move =
    case move of
        Slide from   _ -> from
        Hop   from _ _ -> from
        Take  from _ _ -> from


to : Move -> (Int, Int)
to move =
    case move of
        Slide _   to -> to
        Hop   _ _ to -> to
        Take  _ _ to -> to


taken : Move -> Maybe (Int, Int)
taken move =
    case move of
        Take  _ taken _ -> Just taken
        default         -> Nothing


-- Return where the piece specified could move to (if anywhere)
allowedEnds : Board.Piece -> Board.Board -> (Int, Int) -> List (Int, Int)
allowedEnds side board (xpos, ypos) =
    List.map
        to
        ( List.filter
            ( \move ->
                let
                    (x, y) = from move
                in
                    x == xpos && y == ypos
            )
            ( allowedMoves side board )
        )


allowedMoves : Board.Piece -> Board.Board -> List Move
allowedMoves side board =
    case whoWon board of
        Board.XPiece -> []
        Board.OPiece -> []
        default ->
            let
                ourPieces =
                    List.filter
                        (\pos -> Board.pieceAt pos board == side)
                        Board.positions
            in
                anyTakesMeansOnlyTakes
                    <| List.concat <| List.map
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


anyTakesMeansOnlyTakes : List Move -> List Move
anyTakesMeansOnlyTakes moves =
    let
        onlyTakes = List.filter
            ( \move -> case move of
                Take _ _ _ -> True
                _ -> False
            )
            moves
    in
        if List.isEmpty onlyTakes then
            moves
        else
            onlyTakes


unique : List comparable -> List comparable
unique = Set.toList << Set.fromList


whichCanMove : Board.Piece -> Board.Board -> WhoCanMove
whichCanMove side board =
    case whoWon board of
        Board.XPiece -> Won Board.xPiece
        Board.OPiece -> Won Board.oPiece
        default ->
            CanMovePositions
                <| unique <| List.map from <| allowedMoves side board


whoWon : Board.Board -> Board.Piece
whoWon board =
    let
        (rt, rm, rn, rb) = board.pieces
        (t0, t1, t2, t3) = rt
        (b0, b1, b2, b3) = rb
        (m0, m1, m2, m3) = rm
        (n0, n1, n2, n3) = rn
        all = [t0, t1, t2, t3, m0, m1, m2, m3, n0, n1, n2, n3, b0, b1, b2, b3]
    in
        if List.member Board.oPiece [t0, t1, t2, t3] then
            Board.oPiece
        else if List.member Board.xPiece [b0, b1, b2, b3] then
            Board.xPiece
        else if not (List.member Board.xPiece all) then
            Board.oPiece
        else if not (List.member Board.oPiece all) then
            Board.xPiece
        else
            Board.noPiece
