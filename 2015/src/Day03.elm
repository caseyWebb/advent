module Day03 exposing (..)

import Parser exposing (Parser)
import Parser.Extra
import Set


parser : Parser Int
parser =
    let
        origin =
            ( 0, 0 )
    in
    Parser.loop ( 0, ( origin, origin ), Set.singleton origin )
        (\( i, ( santa, robo ), acc ) ->
            let
                step next =
                    Parser.Loop
                        ( i + 1
                        , if modBy 2 i == 0 then
                            ( next, robo )

                          else
                            ( santa, next )
                        , Set.insert next acc
                        )

                ( x, y ) =
                    if modBy 2 i == 0 then
                        santa

                    else
                        robo
            in
            Parser.oneOf
                [ Parser.Extra.const "^" (step ( x + 1, y ))
                , Parser.Extra.const "v" (step ( x - 1, y ))
                , Parser.Extra.const ">" (step ( x, y + 1 ))
                , Parser.Extra.const "<" (step ( x, y - 1 ))
                , Parser.end |> Parser.map (always (Parser.Done (Set.size acc)))
                ]
        )


solve : String -> Int
solve =
    Parser.run parser >> Result.withDefault -1
