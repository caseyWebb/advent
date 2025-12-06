module List.Extra exposing (find, split, transpose)


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                find predicate xs


transpose : List (List a) -> List (List a)
transpose listOfLists =
    List.foldr (List.map2 (::))
        (List.repeat
            (case listOfLists of
                [] ->
                    0

                x :: _ ->
                    List.length x
            )
            []
        )
        listOfLists


split : a -> List a -> List (List a)
split separator =
    let
        helper ( accCurrent, accOut ) list =
            case list of
                [] ->
                    accCurrent :: accOut |> List.reverse

                x :: rest ->
                    if x == separator then
                        helper ( [], accCurrent :: accOut ) rest

                    else
                        helper ( x :: accCurrent, accOut ) rest
    in
    helper ( [], [] )
