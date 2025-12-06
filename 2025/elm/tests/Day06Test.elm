module Day06Test exposing (suite)

import Day06 exposing (..)
import Expect
import Test exposing (Test, test)


input : String
input =
    String.trim """
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
"""


suite : Test
suite =
    Test.concat
        [ test "parse" <|
            \_ ->
                Expect.equal (parse input)
                    [ ( Multiply, [ 356, 24, 1 ] )
                    , ( Add, [ 8, 248, 369 ] )
                    , ( Multiply, [ 175, 581, 32 ] )
                    , ( Add, [ 4, 431, 623 ] )
                    ]
        , test "solve" <|
            \_ ->
                Expect.equal (solve (parse input)) 3263827
        ]
