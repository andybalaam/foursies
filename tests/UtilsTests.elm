module UtilsTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Utils


all : Test
all =
    describe "Tests of the test utilities"
        [ test "Empty lists are equal" emptyListsAreEqual
        , test "Non-empty left list does not equal empty" nonEmptyLeftNotEqualEmpty
        , test "Non-empty right list does not equal empty" nonEmptyRightNotEqualEmpty
        , test "Equal non-empty lists are equal" equalNonEmpty
        , test "Non-equal lists of same length are not equal" nonEqualSameLength
        ]


emptyListsAreEqual : () -> Expect.Expectation
emptyListsAreEqual =
    \() -> Expect.equal Expect.pass <| Utils.equalExceptOrder [] []


nonEmptyLeftNotEqualEmpty : () -> Expect.Expectation
nonEmptyLeftNotEqualEmpty =
    \() -> Expect.equal
        (Expect.fail "First is longer than second")
        <| Utils.equalExceptOrder ["a"] []


nonEmptyRightNotEqualEmpty : () -> Expect.Expectation
nonEmptyRightNotEqualEmpty =
    \() -> Expect.equal
        (Expect.fail "Second is longer than first")
        <| Utils.equalExceptOrder [] [1]


equalNonEmpty : () -> Expect.Expectation
equalNonEmpty =
    \() -> Expect.equal
        Expect.pass
        <| Utils.equalExceptOrder [1, 6, 4] [1, 6, 4]


nonEqualSameLength : () -> Expect.Expectation
nonEqualSameLength =
    \() -> Expect.equal
        (Expect.fail "5 is not in right")
        <| Utils.equalExceptOrder [3, 2, 1, 5] [3, 2, 1, 0]
