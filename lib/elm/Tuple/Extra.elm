module Tuple.Extra exposing (apply, swap, triple)


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


apply : (a -> b -> c) -> ( a, b ) -> c
apply f ( a, b ) =
    f a b


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )
