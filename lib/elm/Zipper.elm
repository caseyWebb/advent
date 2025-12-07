module Zipper exposing (Zipper, current, fromList, stepWith, toList)


type Zipper a
    = Zipper (List a) a (List a)


current : Zipper a -> a
current (Zipper _ c _) =
    c


stepWith : (a -> a -> a) -> Zipper a -> Zipper a
stepWith f (Zipper l c r) =
    case ( c, r ) of
        ( a, b :: rest ) ->
            Zipper (a :: l) (f a b) rest

        _ ->
            Zipper l c r


fromList : List a -> Maybe (Zipper a)
fromList list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just (Zipper [] head tail)


toList : Zipper a -> List a
toList (Zipper l c r) =
    List.foldl (::) l (c :: r)
