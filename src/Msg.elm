module Msg exposing (Msg(..))

import Model

type Msg =
    Resize Int Int
    | ChoosePlayer Model.Side
    | ChangePlayer Model.Side Model.Player
