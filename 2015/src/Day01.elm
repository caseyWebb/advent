module Day01 exposing (..)

import Parser exposing (Parser)
import Parser.Extra


parser : Parser Int
parser =
    let
        endIfBasement i floor =
            if floor == -1 then
                Parser.Done i

            else
                Parser.Loop ( i + 1, floor )
    in
    Parser.loop ( 1, 0 )
        (\( i, floor ) ->
            Parser.oneOf
                [ Parser.Extra.const "(" (endIfBasement i (floor + 1))
                , Parser.Extra.const ")" (endIfBasement i (floor - 1))
                ]
        )


solve : String -> Result (List Parser.DeadEnd) Int
solve =
    Parser.run parser
