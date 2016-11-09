module Utils exposing (allEqual, equalExceptOrder)

import Expect


allEqual : a -> List a -> Expect.Expectation
allEqual expected actualList =
    case (List.all (\x -> x == expected) actualList) of
        True ->  Expect.pass
        False -> Expect.fail ("Not all equal to " ++ toString expected)


equalExceptOrder : List a -> List a -> Expect.Expectation
equalExceptOrder left right =
    case (left, right) of
        (l :: ls, []) -> Expect.fail ((toString l) ++ " is not in right")
        ([], r :: rs) -> Expect.fail ((toString r) ++ " is not in left")
        (l :: ls, r :: rs ) ->
            let
                rightWithoutL = List.filter (\x -> x /= l) right
            in
                if right == rightWithoutL then
                    Expect.fail <| (toString l) ++ " is not in right"
                else
                    equalExceptOrder ls rightWithoutL
        ([], []) -> Expect.pass
