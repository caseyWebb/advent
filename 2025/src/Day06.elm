module Day06 exposing (..)

import List.Extra
import Maybe.Extra
import Parser
import Parser.Extra


type Operation
    = Add
    | Multiply


parseOps : String -> List Operation
parseOps =
    Parser.run
        (Parser.Extra.ssv
            (Parser.oneOf
                [ Parser.Extra.const "+" Add
                , Parser.Extra.const "*" Multiply
                ]
            )
        )
        >> Result.withDefault []



-- parseRow : String -> List Int
-- parseRow =
--     Parser.run (Parser.Extra.spaceSeparatedList Parser.int)
--         >> Result.withDefault []
-- parse : String -> List ( Operation, List Int )


parse input =
    let
        lines =
            String.lines input
    in
    case List.reverse lines of
        ops :: rows ->
            let
                sets =
                    List.map (\row -> String.split "" ("|" ++ row ++ "|")) rows
                        |> List.Extra.transpose
                        |> List.map
                            (List.reverse
                                >> String.join ""
                                >> String.replace "|" ""
                                >> String.trim
                            )
                        |> List.Extra.split ""
                        |> List.filter (\row -> row /= [])
                        |> List.map (List.map String.toInt >> Maybe.Extra.values)
            in
            List.map2 Tuple.pair (parseOps ops) sets

        _ ->
            Debug.todo "CRASH!"


solve : List ( Operation, List Int ) -> Int
solve =
    List.map
        (\( op, nums ) ->
            case op of
                Add ->
                    List.sum nums

                Multiply ->
                    List.product nums
        )
        >> List.sum
