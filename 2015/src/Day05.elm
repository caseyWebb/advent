module Day05 exposing (..)

import Regex


solve : String -> Int
solve =
    String.lines >> List.filter isNice >> List.length


repeatedPairRegex =
    Regex.fromString "(.)(.).*\\1\\2" |> Maybe.withDefault Regex.never


repeatedWithGapRegex =
    Regex.fromString "(.).\\1" |> Maybe.withDefault Regex.never


isNice : String -> Bool
isNice input =
    let
        hasRepeatedLetters =
            Regex.contains repeatedPairRegex input

        hasRepeatedWithGap =
            Regex.contains repeatedWithGapRegex input
    in
    hasRepeatedLetters && hasRepeatedWithGap
