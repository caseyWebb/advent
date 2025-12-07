module Day05 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Parser.Extra


type alias Range =
    ( Int, Int )


parser : Parser (List Range)
parser =
    Parser.loop []
        (\acc ->
            Parser.oneOf
                [ Parser.succeed (\start end -> Parser.Loop (( start, end ) :: acc))
                    |= Parser.int
                    |. Parser.symbol "-"
                    |= Parser.int
                    |. Parser.symbol "\n"
                , Parser.succeed (Parser.Done (List.reverse acc))
                ]
        )
        |. Parser.symbol "\n"
        |. Parser.Extra.lines Parser.int


solve : String -> Int
solve input =
    Parser.run parser input
        |> Result.withDefault []
        |> combineRanges
        |> List.foldl (\( lower, upper ) acc -> acc + (upper - lower) + 1) 0


combineRanges : List Range -> List Range
combineRanges =
    let
        helper acc ranges =
            case ranges of
                [] ->
                    acc

                ( lower, upper ) :: rest ->
                    let
                        ( overlapping, remaining ) =
                            List.partition (\( l, u ) -> l <= upper && lower <= u) rest
                    in
                    case List.unzip overlapping of
                        ( [], [] ) ->
                            helper (( lower, upper ) :: acc) remaining

                        ( lowers, uppers ) ->
                            let
                                minLower =
                                    List.minimum (lower :: lowers) |> Maybe.withDefault lower

                                maxUpper =
                                    List.maximum (upper :: uppers) |> Maybe.withDefault upper
                            in
                            helper acc (( minLower, maxUpper ) :: remaining)
    in
    helper []
