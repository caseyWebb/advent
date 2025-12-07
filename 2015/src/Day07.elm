module Day07 exposing (..)

import Bitwise
import Dict exposing (Dict)
import Html exposing (i)
import Maybe.Extra
import Parser exposing ((|.), (|=))
import Parser.Extra
import Tuple.Extra


type alias Wire =
    String


type Input
    = WireInput Wire
    | DirectInput Int


type Gate
    = And Input Input
    | Or Input Input
    | LShift Input Int
    | RShift Input Int
    | Not Input
    | Source Input


parse : String -> List (Result (List Parser.DeadEnd) ( Gate, Wire ))
parse =
    let
        inputParser =
            Parser.Extra.word
                |> Parser.map
                    (\w ->
                        case String.toInt w of
                            Just i ->
                                DirectInput i

                            Nothing ->
                                WireInput w
                    )
    in
    String.lines
        >> List.map
            (Parser.run
                (Parser.succeed Tuple.pair
                    |= Parser.oneOf
                        [ Parser.succeed Not
                            |. Parser.symbol "NOT"
                            |. Parser.spaces
                            |= inputParser
                        , inputParser
                            |> Parser.andThen
                                (\input ->
                                    Parser.spaces
                                        |> Parser.andThen
                                            (always
                                                (Parser.oneOf
                                                    [ Parser.oneOf
                                                        [ Parser.Extra.const "AND" (And input)
                                                        , Parser.Extra.const "OR" (Or input)
                                                        ]
                                                        |. Parser.spaces
                                                        |= inputParser
                                                    , Parser.oneOf
                                                        [ Parser.Extra.const "LSHIFT" (LShift input)
                                                        , Parser.Extra.const "RSHIFT" (RShift input)
                                                        ]
                                                        |. Parser.spaces
                                                        |= Parser.int
                                                    , Parser.succeed (Source input)
                                                    ]
                                                )
                                            )
                                )
                        ]
                    |. Parser.spaces
                    |. Parser.symbol "->"
                    |. Parser.spaces
                    |= Parser.Extra.word
                )
            )


solveFor : Wire -> List ( Gate, Wire ) -> Int
solveFor wire inputs =
    let
        inputLookup =
            List.map Tuple.Extra.swap inputs |> Dict.fromList
    in
    helper inputLookup wire |> Tuple.second


solvePart1 : List ( Gate, Wire ) -> Int
solvePart1 inputs =
    helper (List.map Tuple.Extra.swap inputs |> Dict.fromList) "a" |> Tuple.second


solvePart2 : List ( Gate, Wire ) -> Int
solvePart2 inputs =
    let
        lookup =
            List.map Tuple.Extra.swap inputs |> Dict.fromList

        ( _, a ) =
            helper lookup "a"

        ( _, newA ) =
            helper (Dict.insert "b" (Source (DirectInput a)) lookup) "a"
    in
    newA


helper : Dict Wire Gate -> Wire -> ( Dict Wire Gate, Int )
helper inputs wire =
    case Dict.get wire inputs |> Maybe.Extra.require ("Dead end: " ++ wire) of
        Source input ->
            source inputs input

        And a b ->
            source2Then inputs a b Bitwise.and

        Or a b ->
            source2Then inputs a b Bitwise.or

        Not a ->
            sourceThen inputs a (Bitwise.xor (2 ^ 16 - 1))

        LShift a b ->
            sourceThen inputs a (Bitwise.shiftLeftBy b)

        RShift a b ->
            sourceThen inputs a (Bitwise.shiftRightBy b)


sourceThen : Dict Wire Gate -> Input -> (Int -> a) -> ( Dict Wire Gate, a )
sourceThen inputs input fn =
    let
        ( updatedInputs, val ) =
            source inputs input
    in
    ( updatedInputs, fn val )


source2Then : Dict Wire Gate -> Input -> Input -> (Int -> Int -> a) -> ( Dict Wire Gate, a )
source2Then inputs input1 input2 fn =
    let
        ( updatedInputs, val1 ) =
            source inputs input1

        ( finallyUpdatedInputs, val2 ) =
            source updatedInputs input2
    in
    ( finallyUpdatedInputs, fn val1 val2 )


source : Dict Wire Gate -> Input -> ( Dict Wire Gate, Int )
source inputs input =
    case input of
        WireInput s ->
            let
                ( updatedDict, value ) =
                    helper inputs s
            in
            ( Dict.insert s (Source (DirectInput value)) updatedDict, value )

        DirectInput value ->
            ( inputs, value )
