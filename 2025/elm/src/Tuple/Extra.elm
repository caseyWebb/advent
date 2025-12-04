module Tuple.Extra exposing (apply)


apply : (a -> b -> c) -> ( a, b ) -> c
apply f ( a, b ) =
    f a b
