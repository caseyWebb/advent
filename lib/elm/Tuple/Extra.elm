module Tuple.Extra exposing (apply, swap)


apply : (a -> b -> c) -> ( a, b ) -> c
apply f ( a, b ) =
    f a b


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )
