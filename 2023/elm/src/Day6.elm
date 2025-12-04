module Day6 exposing (..)


part1SampleInput : List ( Int, Int )
part1SampleInput =
    [ ( 7, 9 ), ( 15, 40 ), ( 30, 200 ) ]


part2SampleInput : ( Float, Float )
part2SampleInput =
    ( 71530, 940200 )


part1Input : List ( Int, Int )
part1Input =
    [ ( 54, 302 ), ( 94, 1476 ), ( 65, 1029 ), ( 92, 1404 ) ]


part2Input : ( Float, Float )
part2Input =
    ( 54946592, 302147610291404 )



-- solve : List ( Int, Int ) -> Int
-- solve =
--     List.map waysToWin >> List.product
--
--
-- waysToWin : ( Int, Int ) -> Int
-- waysToWin ( raceTime, distanceToBeat ) =
--     let
--         findWin =
--             find (distance raceTime >> (<) distanceToBeat)
--
--         low =
--             findWin ((+) 1) 1
--
--         high =
--             findWin ((+) -1) (raceTime - 1)
--     in
--     high - low + 1
--
--
-- find : (a -> Bool) -> (a -> a) -> a -> a
-- find pred next =
--     let
--         iterate i =
--             if pred i then
--                 i
--
--             else
--                 iterate (next i)
--     in
--     iterate
--
--
-- distance : Int -> Int -> Int
-- distance raceTime buttonHoldTime =
--     buttonHoldTime * (raceTime - buttonHoldTime)


solve : ( Float, Float ) -> Int
solve ( t, d ) =
    floor (abs (-t - sqrt (t ^ 2 - 4 * d)) / 2) - ceiling (abs (-t + sqrt (t ^ 2 - 4 * d)) / 2) + 1
