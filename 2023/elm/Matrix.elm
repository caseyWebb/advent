module Matrix exposing (..)

import Array exposing (Array)
import List.Extra as List
import Maybe.Extra as Maybe


type alias Matrix a =
    Array (Array a)


get : ( Int, Int ) -> Matrix a -> Maybe a
get ( row, col ) =
    Array.get row
        >> Maybe.map (Array.get col)
        >> Maybe.join


find : (a -> Bool) -> Matrix a -> Maybe ( Int, Int )
find predicate =
    Array.indexedMap
        (\row -> Array.indexedMap (\col value -> ( row, col, value )) >> Array.toList)
        >> Array.toList
        >> List.concat
        >> List.findMap
            (\( row, col, value ) ->
                if predicate value then
                    Just ( row, col )

                else
                    Nothing
            )
