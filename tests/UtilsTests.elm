module UtilsTests exposing (all)


import Test exposing (describe,test,Test)
import Expect


import Utils


all : Test
all =
    describe "Tests of the test utilities"
        [ test "Empty lists are equal" emptyListsAreEqual
        , test "Non-empty left not equal to empty" nonEmptyLeftNotEqualEmpty
        , test "Non-empty right not equal to empty" nonEmptyRightNotEqualEmpty
        , test "Equal non-empty lists are equal" equalNonEmpty
        , test "Non-equal lists same length are not equal" nonEqualSameLength
        , test "Left longer means not equal" leftLongerMeansNotEqual
        , test "Right longer means not equal" rightLongerMeansNotEqual
        ]


emptyListsAreEqual : () -> Expect.Expectation
emptyListsAreEqual =
    \() -> Expect.equal Expect.pass <| Utils.equalExceptOrder [] []


nonEmptyLeftNotEqualEmpty : () -> Expect.Expectation
nonEmptyLeftNotEqualEmpty =
    \() -> Expect.equal
        (Expect.fail "\"a\" is not in right (left=[\"a\"], right=[])")
        <| Utils.equalExceptOrder ["a"] []


nonEmptyRightNotEqualEmpty : () -> Expect.Expectation
nonEmptyRightNotEqualEmpty =
    \() -> Expect.equal
        (Expect.fail "1 is not in left (left=[], right=[1])")
        <| Utils.equalExceptOrder [] [1]


equalNonEmpty : () -> Expect.Expectation
equalNonEmpty =
    \() -> Expect.equal
        Expect.pass
        <| Utils.equalExceptOrder [6, 1, 4] [1, 6, 4]


nonEqualSameLength : () -> Expect.Expectation
nonEqualSameLength =
    \() -> Expect.equal
        (Expect.fail "5 is not in right (left=[2,3,1,5], right=[3,2,1,0])")
        <| Utils.equalExceptOrder [2, 3, 1, 5] [3, 2, 1, 0]


leftLongerMeansNotEqual : () -> Expect.Expectation
leftLongerMeansNotEqual =
    \() -> Expect.equal
        (Expect.fail "5 is not in right (left=[2,3,1,5], right=[3,2,1])")
        <| Utils.equalExceptOrder [2, 3, 1, 5] [3, 2, 1]


rightLongerMeansNotEqual : () -> Expect.Expectation
rightLongerMeansNotEqual =
    \() -> Expect.equal
        (Expect.fail "0 is not in left (left=[2,3,1], right=[3,2,1,0])")
        <| Utils.equalExceptOrder [2, 3, 1] [3, 2, 1, 0]
