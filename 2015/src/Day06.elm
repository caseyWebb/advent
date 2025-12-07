module Day06 exposing (..)

import Dict
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra


type Action
    = On
    | Off
    | Toggle


type alias Point =
    ( Int, Int )


parser : Parser ( Action, ( Point, Point ) )
parser =
    let
        pointParser =
            Parser.succeed Tuple.pair
                |= Parser.int
                |. Parser.symbol ","
                |= Parser.int
    in
    Parser.succeed Tuple.pair
        |= Parser.oneOf
            [ Parser.Extra.const "turn on" On
            , Parser.Extra.const "turn off" Off
            , Parser.Extra.const "toggle" Toggle
            ]
        |. Parser.spaces
        |= (Parser.succeed Tuple.pair
                |= pointParser
                |. Parser.spaces
                |. Parser.symbol "through"
                |. Parser.spaces
                |= pointParser
           )


solve =
    String.lines
        >> List.map (Parser.run parser >> Result.toMaybe)
        >> Maybe.Extra.values
        >> List.foldl applyAction Dict.empty
        >> Dict.values
        >> List.sum


applyAction ( action, ( ( x1, y1 ), ( x2, y2 ) ) ) acc =
    let
        points =
            List.range (min x1 x2) (max x1 x2)
                |> List.concatMap
                    (\x ->
                        List.range (min y1 y2) (max y1 y2)
                            |> List.map (\y -> ( x, y ))
                    )

        update op =
            List.foldl
                (\point -> Dict.update point (Maybe.withDefault 0 >> op >> Just))
                acc
                points
    in
    case action of
        On ->
            update ((+) 1)

        Off ->
            update ((+) -1 >> max 0)

        Toggle ->
            update ((+) 2)
