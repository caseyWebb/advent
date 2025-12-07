module Utils exposing (parseAndSolve)

import Parser exposing (Parser)


parseAndSolve : Parser a -> (a -> b) -> String -> Result (List Parser.DeadEnd) b
parseAndSolve parser fn =
    String.trim
        >> Parser.run parser
        >> Result.map fn
