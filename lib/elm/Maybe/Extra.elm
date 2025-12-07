module Maybe.Extra exposing (require, values)


values : List (Maybe a) -> List a
values =
    List.filterMap identity


require : String -> Maybe a -> a
require msg maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.todo msg
