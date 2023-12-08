module Math exposing (..)


lcm : List Int -> Int
lcm =
    let
        iterate prime factors nums =
            if List.all isPrime nums then
                List.product (Debug.log "factors" List.append factors nums)

            else
                let
                    ( updatedNums, modified ) =
                        List.map
                            (\n ->
                                if modBy prime n == 0 then
                                    ( n // prime, True )

                                else
                                    ( n, False )
                            )
                            nums
                            |> List.unzip
                            |> Tuple.mapFirst (List.filter ((/=) 1))
                            |> Tuple.mapSecond (List.any identity)
                in
                if modified then
                    iterate 2 (prime :: factors) updatedNums

                else
                    iterate (nextPrime prime) factors updatedNums
    in
    iterate 2 []


nextPrime : Int -> Int
nextPrime =
    (+) 1
        >> (\n ->
                if isPrime n then
                    n

                else
                    nextPrime n
           )


isPrime : Int -> Bool
isPrime n =
    case n of
        1 ->
            False

        2 ->
            True

        _ ->
            let
                sqrtN =
                    floor (sqrt (toFloat n))

                isDivisibleBy d =
                    modBy d n == 0
            in
            not <| List.any isDivisibleBy (List.range 2 sqrtN)
