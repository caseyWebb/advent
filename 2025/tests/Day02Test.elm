module Day02Test exposing (..)

import Day02
import Expect
import Test exposing (..)


suite : Test
suite =
    Test.describe "Day02"
        [ test "#1" <|
            \_ ->
                Expect.equal
                    (Day02.solve "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")
                    (Ok 4174379265)
        ]
