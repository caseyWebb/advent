module Day03Test exposing (..)

import Day03 exposing (..)
import Expect
import Test exposing (Test, test)


suite : Test
suite =
    Test.concat
        [ test "Case #1" <|
            \_ ->
                Expect.equal (solve "^v") 3
        , test "Case #2" <|
            \_ ->
                Expect.equal (solve "^>v<") 3
        , test "Case #3" <|
            \_ ->
                Expect.equal (solve "^v^v^v^v^v") 11
        ]
