module Parser.Extra exposing (..)

import Parser exposing (..)
import Set


const : String -> a -> Parser a
const str ctor =
    symbol str |> map (always ctor)


alphaNum : Parser String
alphaNum =
    variable
        { start = \c -> Char.isAlphaNum c
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.empty
        }


word : Parser String
word =
    variable
        { start = \c -> Char.isAlpha c
        , inner = \c -> Char.isAlpha c
        , reserved = Set.empty
        }


list : Parser a -> Parser (List a)
list parser =
    loop []
        (\acc ->
            spaces
                |> andThen
                    (always
                        (oneOf
                            [ parser |> map (\item -> Loop (item :: acc))
                            , succeed (Done (List.reverse acc))
                            ]
                        )
                    )
        )


repeat : Int -> Parser a -> Parser (List a)
repeat n parser =
    loop []
        (\acc ->
            if List.length acc < n then
                parser |> map (\item -> Loop (item :: acc))

            else
                succeed (Done (List.reverse acc))
        )
