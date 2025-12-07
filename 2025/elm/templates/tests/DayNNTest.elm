module DayNNTest exposing (suite)

import DayNN exposing (..)
import Expect
import Test exposing (Test, test)


input : String
input =
    String.trim """
"""


suite : Test
suite =
    Test.concat
        [ test "parse" <|
            \_ ->
                Expect.equal (parse input) (Debug.todo "")
        , test "solve" <|
            \_ ->
                Expect.equal (solve (parse input)) (Debug.todo "")
        ]
