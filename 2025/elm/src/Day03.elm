module Day03 exposing (..)

import Maybe.Extra


parse : String -> List (List Int)
parse =
    String.trim >> String.split "\n" >> List.map (String.split "" >> List.map String.toInt >> Maybe.Extra.values)


solve : String -> Int
solve =
    parse >> List.map solveLine >> List.sum


solveLine : List Int -> Int
solveLine digits =
    solveLineHelper [] 12 (List.length digits) (List.reverse digits) |> List.reverse |> digitsToInt


solveLineHelper : List Int -> Int -> Int -> List Int -> List Int
solveLineHelper acc digitN maxIndex digits =
    if digitN == 0 then
        acc

    else
        let
            ( nextDigit, newMaxIndex ) =
                List.take maxIndex digits
                    |> List.drop (digitN - 1)
                    |> indexedFindLastMax
                    |> Tuple.mapSecond ((+) (digitN - 1))
        in
        solveLineHelper (nextDigit :: acc) (digitN - 1) newMaxIndex digits


indexedFindLastMax : List number -> ( number, number )
indexedFindLastMax digits =
    indexedFindLastMaxHelper digits 0 ( 0, 0 )


indexedFindLastMaxHelper : List comparable -> number -> ( comparable, number ) -> ( comparable, number )
indexedFindLastMaxHelper digits index ( selected, selectedIndex ) =
    case digits of
        [] ->
            ( selected, selectedIndex )

        x :: rest ->
            indexedFindLastMaxHelper
                rest
                (index + 1)
                (if x >= selected then
                    ( x, index )

                 else
                    ( selected, selectedIndex )
                )


digitsToInt : List Int -> Int
digitsToInt digits =
    List.foldl (\digit acc -> acc * 10 + digit) 0 digits
