module Day02 exposing (solve)

import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Tuple.Extra
import Utils


type alias Range =
    ( Int, Int )


parser : Parser (List Range)
parser =
    Parser.Extra.csv
        (Parser.succeed Tuple.pair
            |= Parser.int
            |. Parser.symbol "-"
            |= Parser.int
        )


solve : String -> Result (List Parser.DeadEnd) Int
solve =
    Utils.parseAndSolve parser
        (List.concatMap (Tuple.Extra.apply List.range) >> List.filter isRepeatedDigits >> List.sum)


isRepeatedDigits : Int -> Bool
isRepeatedDigits num =
    let
        digitString =
            String.fromInt num

        digitCount =
            String.length digitString

        chars =
            String.toList digitString

        isRepeatedDigitsOfSize : Int -> Bool
        isRepeatedDigitsOfSize size =
            let
                digitCountIsDivisibleBySize =
                    modBy size digitCount == 0
            in
            if digitCountIsDivisibleBySize then
                let
                    segments =
                        segmentString size chars []
                in
                allSegmentsEqual segments

            else
                False
    in
    List.range 1 (digitCount // 2)
        |> List.any isRepeatedDigitsOfSize


segmentString : Int -> List Char -> List (List Char) -> List (List Char)
segmentString size chars acc =
    let
        segment =
            List.take size chars

        rest =
            List.drop size chars
    in
    if List.length chars == 0 then
        acc

    else
        segmentString size rest (segment :: acc)


allSegmentsEqual : List a -> Bool
allSegmentsEqual segments =
    case segments of
        a :: b :: rest ->
            if a == b then
                allSegmentsEqual (b :: rest)

            else
                False

        _ ->
            True
