module Iterator exposing (..)


type Iterator a
    = Iterator (Thunk ( a, Iterator a ))


type alias Thunk a =
    () -> a


fromList : List a -> Iterator (Maybe a)
fromList list =
    case list of
        [] ->
            Iterator (\() -> ( Nothing, fromList [] ))

        a :: rest ->
            Iterator (\() -> ( Just a, fromList rest ))


next : Iterator a -> ( a, Iterator a )
next (Iterator thunk) =
    thunk ()


take : Int -> Iterator a -> List a
take n iterator =
    if n <= 0 then
        []

    else
        let
            ( a, rest ) =
                next iterator
        in
        a :: take (n - 1) rest


scan2 : (a -> a -> b) -> Iterator a -> Iterator b
scan2 fn iterator =
    let
        ( a, ( b, rest ) ) =
            next iterator |> Tuple.mapSecond next
    in
    Iterator (\() -> ( fn a b, scan2 fn (Iterator (\() -> ( b, rest ))) ))
