module Day01Test exposing (..)

import Day01
import Expect
import Test exposing (..)


suite : Test
suite =
    Test.describe "Day01"
        [ test "#1" <|
            \_ ->
                Expect.equal
                    (Day01.solve
                        """
                        L68
                        L30
                        R48
                        L5
                        R60
                        L55
                        L1
                        L99
                        R14
                        L82
                        """
                    )
                    (Ok 6)
        , test "#2" <|
            \_ ->
                Expect.equal
                    (Day01.solve
                        """
                        R49
                        R1
                        R100
                        R1
                        L1
                        L100
                        """
                    )
                    (Ok 4)
        ]
