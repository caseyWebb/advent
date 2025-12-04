module Day01 exposing (solve)

import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Utils


type Direction
    = Left
    | Right


parser : Parser (List ( Direction, Int ))
parser =
    Parser.Extra.lines
        (Parser.succeed Tuple.pair
            |= Parser.oneOf
                [ Parser.Extra.const "L" Left
                , Parser.Extra.const "R" Right
                ]
            |= Parser.int
        )


solve : String -> Result (List Parser.DeadEnd) Int
solve =
    Utils.parseAndSolve parser
        (List.foldl
            (\( direction, distance ) ( currentPosition, zeroCount ) ->
                let
                    op =
                        case direction of
                            Left ->
                                (-)

                            Right ->
                                (+)

                    fullRotations =
                        distance // 100

                    distanceAfterFullRotations =
                        modBy 100 distance

                    updatedRawPosition =
                        op currentPosition distanceAfterFullRotations

                    updatedPosition =
                        modBy 100 updatedRawPosition

                    crossedZero =
                        currentPosition /= 0 && (updatedRawPosition <= 0 || updatedRawPosition >= 100)

                    updatedZeroCount =
                        zeroCount
                            + fullRotations
                            + (if crossedZero then
                                1

                               else
                                0
                              )
                in
                ( updatedPosition, updatedZeroCount )
            )
            ( 50, 0 )
        )
        >> Result.map Tuple.second
