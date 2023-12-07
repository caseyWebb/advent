module Parser.Extra exposing (..)

import Parser exposing (..)


const : String -> a -> Parser a
const str ctor =
    symbol str |> map (always ctor)


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
