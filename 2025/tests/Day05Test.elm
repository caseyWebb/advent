module Day05Test exposing (..)

import Day05 exposing (..)
import Expect
import Parser
import Test exposing (..)


input : String
input =
    String.trim """
3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""


suite : Test
suite =
    Test.describe "Day05"
        [ test "parser" <|
            \_ ->
                Expect.equal
                    (Parser.run parser input)
                    (Ok
                        [ ( 3, 5 )
                        , ( 10, 14 )
                        , ( 16, 20 )
                        , ( 12, 18 )
                        ]
                    )
        , test "combineRanges" <|
            \_ ->
                Expect.equal
                    (combineRanges
                        [ ( 3, 5 )
                        , ( 10, 14 )
                        , ( 16, 20 )
                        , ( 12, 18 )
                        , ( 25, 30 )
                        , ( 30, 35 )
                        , ( 35, 40 )
                        ]
                        |> List.sort
                    )
                    [ ( 3, 5 )
                    , ( 10, 20 )
                    , ( 25, 40 )
                    ]
        , test "solve" <|
            \_ ->
                Expect.equal (solve input) 14
        ]
