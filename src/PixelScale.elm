module PixelScale exposing
    ( backgroundWidth
    , boardScale
    , boardWidth
    , boardDistance
    , gridDistance
    , piecesScale
    )


import Mouse


import Model


boardWidth : Model.Model -> Int
boardWidth model =
    let
        minD = Basics.min model.screen.width model.screen.height
    in
        round <| (toFloat minD) * 0.9


-- Given a mouse start and mouse end return number of grid squares` we have
-- moved to by moving from the mouse start to the mouse end.
gridDistance : Model.Model -> Mouse.Position -> Mouse.Position
    -> (Int, Int)
gridDistance model dragMouseStart mouseEnd =
    let
        movedX = mouseEnd.x - dragMouseStart.x
        movedY = mouseEnd.y - dragMouseStart.y
        sc = pieceWidth * (boardScale model) * piecesScale
    in
        (round ((toFloat movedX) / sc), round ((toFloat movedY / sc)))


-- Given a movement in pixels of the mouse, return the distance
-- in scaled units equivalent to the units of the SVGs on the
-- board.  So if our mouse was moved left by the width of one piece,
-- we will return (-20, 0).
-- This allows us to adjust the position of a piece when it is
-- being dragged: we are given the mouse positions but we need
-- to move the piece within its own co-ordinate system.
boardDistance : Model.Model -> Mouse.Position -> Mouse.Position
    -> (Float, Float)
boardDistance model startPx endPx =
    let
        sc = (boardScale model) * piecesScale
        pixelsMovedX = endPx.x - startPx.x
        pixelsMovedY = endPx.y - startPx.y
        movedX = (toFloat pixelsMovedX) / sc
        movedY = (toFloat pixelsMovedY) / sc
    in
        (movedX, movedY)


piecesScale : Float
piecesScale = 2.198


boardScale : Model.Model -> Float
boardScale model =
    (toFloat (boardWidth model)) / backgroundWidth


backgroundWidth : Float
backgroundWidth = 200


-- The width, in piece co-ords, of a piece
pieceWidth : Float
pieceWidth = 20
