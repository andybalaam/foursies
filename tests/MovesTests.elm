module MovesTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Utils


import Board
import Moves


all : Test
all =
    describe "Tests of the move calculator"
        [ test "Normally all O pieces can move" allOPiecesCanMove
        , test "Normally all X pieces can move" allXPiecesCanMove
        , test "Generate forward moves with no jumps" forwardMovesNoJumps
        , test "Generate backward moves with no jumps" backwardMovesNoJumps
        , test "Generate hops and slides" hopsAndSlides
        , test "Generate all hops down-right" allHopsDownRight
        , test "Generate all hops up-left" allHopsUpLeft
        ]


allOPiecesCanMove : () -> Expect.Expectation
allOPiecesCanMove =
    Utils.forBoard
        "X..."
        "...."
        "...."
        "O.O."
        <| \board -> Expect.equal
            [(0, 3), (2, 3)]
            ( Moves.whichCanMove Board.oPiece board )


allXPiecesCanMove : () -> Expect.Expectation
allXPiecesCanMove =
    Utils.forBoard
        "...."
        ".X.."
        "...."
        "O.O."
        <| \board -> Expect.equal
            [(1, 1)]
            ( Moves.whichCanMove Board.xPiece board )


forwardMovesNoJumps : () -> Expect.Expectation
forwardMovesNoJumps =
    Utils.forBoard
        "XXXX"
        "...."
        "...."
        "OOOO"
        <| \board -> Utils.equalExceptOrder
            [ (Moves.slide (0, 3) (0, 2))
            , (Moves.slide (0, 3) (1, 2))
            , (Moves.slide (1, 3) (0, 2))
            , (Moves.slide (1, 3) (1, 2))
            , (Moves.slide (1, 3) (2, 2))
            , (Moves.slide (2, 3) (1, 2))
            , (Moves.slide (2, 3) (2, 2))
            , (Moves.slide (2, 3) (3, 2))
            , (Moves.slide (3, 3) (2, 2))
            , (Moves.slide (3, 3) (3, 2))
            ]
            ( Moves.allowedMoves Board.oPiece board )


backwardMovesNoJumps : () -> Expect.Expectation
backwardMovesNoJumps =
    Utils.forBoard
        "...."
        ".X.O"
        "...."
        "...."
        <| \board -> Utils.equalExceptOrder
            [ (Moves.slide (1, 1) (1, 0))
            , (Moves.slide (1, 1) (2, 0))
            , (Moves.slide (1, 1) (2, 1))
            , (Moves.slide (1, 1) (2, 2))
            , (Moves.slide (1, 1) (1, 2))
            , (Moves.slide (1, 1) (0, 2))
            , (Moves.slide (1, 1) (0, 1))
            , (Moves.slide (1, 1) (0, 0))
            ]
            ( Moves.allowedMoves Board.xPiece board )


hopsAndSlides: () -> Expect.Expectation
hopsAndSlides =
    Utils.forBoard
        ".X.."
        ".X.."
        "...."
        "...O"
        <| \board -> Utils.equalExceptOrder
            [ (Moves.slide (1, 0) (2, 0))
            , (Moves.slide (1, 0) (2, 1))
            , (Moves.slide (1, 0) (0, 1))
            , (Moves.slide (1, 0) (0, 0))
            , (Moves.hop   (1, 0) (1, 1) (1, 2))
            , (Moves.slide (1, 1) (2, 0))
            , (Moves.slide (1, 1) (2, 1))
            , (Moves.slide (1, 1) (2, 2))
            , (Moves.slide (1, 1) (1, 2))
            , (Moves.slide (1, 1) (0, 2))
            , (Moves.slide (1, 1) (0, 1))
            , (Moves.slide (1, 1) (0, 0))
            ]
            ( Moves.allowedMoves Board.xPiece board )


allHopsDownRight: () -> Expect.Expectation
allHopsDownRight =
    let
        hops11 = \pos -> case pos of
            Moves.Hop (1, 1) _ _ -> True
            _ -> False
    in
        Utils.forBoard
            ".X.."
            ".OO."
            ".OO."
            "...."
            <| \board -> Utils.equalExceptOrder
                [ (Moves.hop   (1, 1) (2, 1) (3, 1))
                , (Moves.hop   (1, 1) (2, 2) (3, 3))
                , (Moves.hop   (1, 1) (1, 2) (1, 3))
                ]
                ( List.filter hops11 <| Moves.allowedMoves Board.oPiece board )


allHopsUpLeft: () -> Expect.Expectation
allHopsUpLeft =
    let
        hops22 = \pos -> case pos of
            Moves.Hop (2, 2) _ _ -> True
            _ -> False
    in
        Utils.forBoard
            "...."
            ".XX."
            ".XX."
            ".O.."
            <| \board -> Utils.equalExceptOrder
                [ (Moves.hop   (2, 2) (2, 1) (2, 0))
                , (Moves.hop   (2, 2) (1, 1) (0, 0))
                , (Moves.hop   (2, 2) (1, 2) (0, 2))
                ]
                ( List.filter hops22 <| Moves.allowedMoves Board.xPiece board )
