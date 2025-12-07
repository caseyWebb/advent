module Day07Test exposing (suite)

import Day07 exposing (..)
import Expect
import Maybe.Extra
import Test exposing (Test, test)


input : String
input =
    String.trim """
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
i -> j
1 AND y -> z
ea AND eb -> ed
"""


expectedOutputs : List ( String, Int )
expectedOutputs =
    [ ( "d", 72 )
    , ( "e", 507 )
    , ( "f", 492 )
    , ( "g", 114 )
    , ( "h", 65412 )
    , ( "i", 65079 )
    , ( "j", 65079 )
    , ( "x", 123 )
    , ( "y", 456 )
    ]


suite : Test
suite =
    Test.concat
        ((test "parse" <|
            \_ ->
                Expect.equal (parse input)
                    [ Ok ( Source (DirectInput 123), "x" )
                    , Ok ( Source (DirectInput 456), "y" )
                    , Ok ( And (WireInput "x") (WireInput "y"), "d" )
                    , Ok ( Or (WireInput "x") (WireInput "y"), "e" )
                    , Ok ( LShift (WireInput "x") 2, "f" )
                    , Ok ( RShift (WireInput "y") 2, "g" )
                    , Ok ( Not (WireInput "x"), "h" )
                    , Ok ( Not (WireInput "y"), "i" )
                    , Ok ( Source (WireInput "i"), "j" )
                    , Ok ( And (DirectInput 1) (WireInput "y"), "z" )
                    , Ok ( And (WireInput "ea") (WireInput "eb"), "ed" )
                    ]
         )
            :: List.map mkTest expectedOutputs
        )


mkTest : ( String, Int ) -> Test
mkTest ( name, expected ) =
    test ("solveFor " ++ name) <|
        \_ ->
            Expect.equal (solveFor name (parse input |> List.map (Result.toMaybe >> Maybe.Extra.require "Parser Error!"))) expected
