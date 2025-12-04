module Tuple.Extra exposing (..)


mapSame : (a -> b) -> ( a, a ) -> ( b, b )
mapSame fn =
    Tuple.mapBoth fn fn
