module Msg exposing (Msg(..))

import Mouse

import Model

type Msg =
    Resize Int Int
    | ChoosePlayer Model.Side
    | ChangePlayer Model.Side Model.Player
    | MouseMove Mouse.Position
    | DragStart Int Int
    | DragStop
    | Touched Int Int
    | Untouched
