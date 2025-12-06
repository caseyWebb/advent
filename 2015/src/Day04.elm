module Day04 exposing (..)

import MD5


input : String
input =
    "iwrupvqb"


solve =
    helper 0


helper i =
    let
        md5 =
            MD5.hex (input ++ String.fromInt i)
    in
    if String.startsWith "000000" md5 then
        i

    else
        helper (i + 1)
