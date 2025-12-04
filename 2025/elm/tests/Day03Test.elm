module Day03Test exposing (..)

import Day03
import Expect
import Test exposing (..)


suite : Test
suite =
    Test.describe "Day03"
        [ test "#1" <|
            \_ ->
                Expect.equal
                    (Day03.solve
                        """
                        987654321111111
                        811111111111119
                        234234234234278
                        818181911112111
                        """
                    )
                    3121910778619
        ]
