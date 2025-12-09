module Parser.Extra exposing (chars, const, csv, hex, lines, ssv, word)

import Parser exposing (..)


hex : Parser Char
hex =
    oneOf
        [ symbol "0" |> map (\_ -> '0')
        , symbol "1" |> map (\_ -> '1')
        , symbol "2" |> map (\_ -> '2')
        , symbol "3" |> map (\_ -> '3')
        , symbol "4" |> map (\_ -> '4')
        , symbol "5" |> map (\_ -> '5')
        , symbol "6" |> map (\_ -> '6')
        , symbol "7" |> map (\_ -> '7')
        , symbol "8" |> map (\_ -> '8')
        , symbol "9" |> map (\_ -> '9')
        , symbol "A" |> map (\_ -> 'A')
        , symbol "B" |> map (\_ -> 'B')
        , symbol "C" |> map (\_ -> 'C')
        , symbol "D" |> map (\_ -> 'D')
        , symbol "E" |> map (\_ -> 'E')
        , symbol "F" |> map (\_ -> 'F')
        ]


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
