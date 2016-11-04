module Utils exposing (allEqual)

import Expect


allEqual : a -> List a -> Expect.Expectation
allEqual expected actualList =
    case (List.all (\x -> x == expected) actualList) of
        True ->  Expect.pass
        False -> Expect.fail ("Not all equal to " ++ toString expected)
