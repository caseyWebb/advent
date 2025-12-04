module Maybe.Extra exposing (values)


values : List (Maybe a) -> List a
values =
    List.filterMap identity
