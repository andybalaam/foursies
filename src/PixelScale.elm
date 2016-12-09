module PixelScale exposing
    ( boardScale
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


gridDistance : Model.Model -> Mouse.Position -> Mouse.Position -> (Int, Int)
gridDistance model startPx endPx =
    let
        (xPx, yPx) = boardDistance model startPx endPx
        w = toFloat <| boardWidth model
    in
        ((round (xPx / w)), (round (yPx / w)))


piecesScale : Float
piecesScale = 2.198


boardScale : Model.Model -> Float
boardScale model =
    (toFloat (boardWidth model)) / 200


