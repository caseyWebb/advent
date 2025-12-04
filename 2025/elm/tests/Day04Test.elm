module Day04Test exposing (..)

import Day04
import Expect
import Test exposing (..)


suite : Test
suite =
    Test.describe "Day04"
        [ test "#1" <|
            \_ ->
                Expect.equal
                    (Day04.solve
                        """
                        ..@@.@@@@.
                        @@@.@.@.@@
                        @@@@@.@.@@
                        @.@@@@..@.
                        @@.@@@@.@@
                        .@@@@@@@.@
                        .@.@.@.@@@
                        @.@@@.@@@@
                        .@@@@@@@@.
                        @.@.@@@.@.
                        """
                    )
                    43
        ]
