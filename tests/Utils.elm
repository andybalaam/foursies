module Utils exposing (allEqual, doForBoard, equalExceptOrder, forBoard)


import Expect


import Board


allEqual : a -> List a -> Expect.Expectation
allEqual expected actualList =
    case (List.all (\x -> x == expected) actualList) of
        True ->  Expect.pass
        False -> Expect.fail ("Not all equal to " ++ toString expected)


equalExceptOrder : List a -> List a -> Expect.Expectation
equalExceptOrder left right =
    case doEqualExceptOrder left right of
        Err e -> Expect.fail
            (  e
            ++ " (left="
            ++ (toString left)
            ++ ", right="
            ++ (toString right)
            ++ ")"
            )
        Ok _ -> Expect.pass


doEqualExceptOrder : List a -> List a -> Result String ()
doEqualExceptOrder left right =
    case (left, right) of
        (l :: ls, []) -> Err ((toString l) ++ " is not in right")
        ([], r :: rs) -> Err ((toString r) ++ " is not in left")
        (l :: ls, r :: rs ) ->
            let
                rightWithoutL = List.filter (\x -> x /= l) right
            in
                if right == rightWithoutL then
                    Err <| (toString l) ++ " is not in right"
                else
                    doEqualExceptOrder ls rightWithoutL
        ([], []) -> Ok ()


forBoard :
    String -> String -> String -> String ->
        (Board.Board -> Expect.Expectation) ->
        (() -> Expect.Expectation)
forBoard s1 s2 s3 s4 fn =
    \() ->
        doForBoard s1 s2 s3 s4 fn


doForBoard :
    String -> String -> String -> String ->
        (Board.Board -> Expect.Expectation) -> Expect.Expectation
doForBoard s1 s2 s3 s4 fn =
    let
        board = Board.parse <| Board.strings s1 s2 s3 s4
    in
        case board of
            Err e -> Expect.fail e
            Ok b -> fn b
