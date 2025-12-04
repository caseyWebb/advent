module Day04 exposing (..)

import Set exposing (Set)


parse : String -> List (List Char)
parse =
    String.trim >> String.lines >> List.map (String.trim >> String.toList)


solve : String -> Int
solve input =
    let
        helper count locations =
            let
                toRemove =
                    Set.filter
                        (\coords ->
                            if countNeighbors locations coords < 4 then
                                True

                            else
                                False
                        )
                        locations

                numToRemove =
                    Set.size toRemove
            in
            if numToRemove == 0 then
                count

            else
                helper (count + numToRemove) (Set.diff locations toRemove)
    in
    helper 0
        (Set.fromList <|
            List.concatMap
                (\( y, row ) ->
                    List.filterMap
                        (\( x, cell ) ->
                            case cell of
                                '@' ->
                                    Just ( x, y )

                                _ ->
                                    Nothing
                        )
                        (List.indexedMap Tuple.pair row)
                )
                (List.indexedMap Tuple.pair (parse input))
        )


countNeighbors : Set ( Int, Int ) -> ( Int, Int ) -> Int
countNeighbors locations ( x, y ) =
    List.length <|
        List.filter
            (\coords ->
                if Set.member coords locations then
                    True

                else
                    False
            )
            [ ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            ]
