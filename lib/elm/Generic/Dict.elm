module Generic.Dict exposing (..)

import Dict as Base


type Dict k v
    = Dict (Base.Dict String ( k, v )) (k -> String)


empty : (k -> String) -> Dict k v
empty =
    Dict Base.empty


fromList : (k -> String) -> List ( k, v ) -> Dict k v
fromList toComparable list =
    Dict (Base.fromList (List.map (\( k, v ) -> ( toComparable k, ( k, v ) )) list)) toComparable


toList : Dict k v -> List ( k, v )
toList (Dict dict _) =
    Base.toList dict |> List.map Tuple.second


get : k -> Dict k v -> Maybe v
get key (Dict dict toComparable) =
    Base.get (toComparable key) dict |> Maybe.map Tuple.second


insert : k -> v -> Dict k v -> Dict k v
insert key value (Dict dict toComparable) =
    Dict (Base.insert (toComparable key) ( key, value ) dict) toComparable


update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key updateFn (Dict dict toComparable) =
    Dict (Base.update (toComparable key) (Maybe.map Tuple.second >> updateFn >> Maybe.map (Tuple.pair key)) dict) toComparable


values : Dict k v -> List v
values =
    toList >> List.map Tuple.second


merge :
    (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> Dict k a
    -> Dict k b
    -> result
    -> result
merge mergeLeft mergeBoth mergeRight (Dict leftDict _) (Dict rightDict _) =
    Base.merge
        (\_ ( k, a ) -> mergeLeft k a)
        (\_ ( k, a ) ( _, b ) -> mergeBoth k a b)
        (\_ ( k, b ) -> mergeRight k b)
        leftDict
        rightDict
