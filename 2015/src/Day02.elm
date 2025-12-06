module Day02 exposing (..)

import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)


type Dimensions
    = Dimensions Int Int Int


parser : Parser Dimensions
parser =
    Parser.succeed Dimensions
        |= Parser.int
        |. Parser.symbol "x"
        |= Parser.int
        |. Parser.symbol "x"
        |= Parser.int


solve : (Dimensions -> Int) -> String -> Int
solve fn =
    String.lines
        >> List.map (Parser.run parser >> Result.map fn >> Result.toMaybe)
        >> Maybe.Extra.values
        >> List.sum


solvePartOne : String -> Int
solvePartOne =
    solve getWrappingPaperSqFt


solvePartTwo : String -> Int
solvePartTwo =
    solve getRibbonFt


getRibbonFt : Dimensions -> Int
getRibbonFt (Dimensions h w l) =
    let
        a =
            2 * h + 2 * w

        b =
            2 * h + 2 * l

        c =
            2 * w + 2 * l

        volume =
            h * w * l

        smallest =
            min a (min b c)
    in
    smallest + volume


getWrappingPaperSqFt : Dimensions -> Int
getWrappingPaperSqFt (Dimensions h w l) =
    let
        a =
            h * w

        b =
            h * l

        c =
            w * l

        smallest =
            min a (min b c)
    in
    2 * a + 2 * b + 2 * c + smallest
