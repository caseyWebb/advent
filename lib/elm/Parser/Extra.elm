module Parser.Extra exposing (..)

import Parser exposing (..)


const : String -> a -> Parser a
const str ctor =
    symbol str |> map (always ctor)
