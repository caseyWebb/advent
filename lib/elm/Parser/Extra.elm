module Parser.Extra exposing (chars, const, csv, lines, ssv, word)

import Parser exposing (..)


word : Parser String
word =
    getChompedString (chompWhile (\c -> c /= ' ' && c /= '\n'))


{-| Parse a constant value, i.e. turn a string into a type constructor

    type Direction = Left | Right

    Parser.oneOf
      [ Parser.Extra.const "L" Left
      , Parser.Extra.const "R" Right
      ]

-}
const : String -> a -> Parser a
const symbol_ const_ =
    symbol symbol_ |> map (always const_)


lines : Parser a -> Parser (List a)
lines =
    loopWithDelimiter spaces


{-| Parse a SINGLE line of Comma Separated Values (CSV)

Use in conjunction with `lines` to parse a full CSV file.

-}
csv : Parser a -> Parser (List a)
csv =
    loopWithDelimiter (oneOf [ symbol ",", end ])


{-| Parse a SINGLE line of Space Separated Values (SSV)

Use in conjunction with `lines` to parse a full SSV file.

-}
ssv : Parser a -> Parser (List a)
ssv =
    loopWithDelimiter spaces


{-| Parse each character in a string
-}
chars : Parser a -> Parser (List a)
chars =
    loopWithDelimiter (Parser.symbol "")


loopWithDelimiter : Parser ignore -> Parser a -> Parser (List a)
loopWithDelimiter delimiter parser =
    loop []
        (\acc ->
            oneOf
                [ parser |. delimiter |> map (\value -> Loop (value :: acc))
                , succeed (Done (List.reverse acc))
                ]
        )
