module Main where

import Data.List (sort)
import Data.Maybe

data AutocompleteResult = Invalid Char | Completion String

main :: IO ()
main = do
  --   contents <- readFile "input.test.txt"
  contents <- readFile "input.txt"

  print $ scoreInvalid $ lines contents
  print $ scoreAutocomplete $ lines contents

  return ()

scoreInvalid :: [String] -> Int
scoreInvalid xs =
  sum $
    map
      ( \x -> case autocomplete x of
          Invalid ')' -> 3
          Invalid ']' -> 57
          Invalid '}' -> 1197
          Invalid '>' -> 25137
          _ -> 0
      )
      xs

scoreAutocomplete :: [String] -> Int
scoreAutocomplete xs =
  median $
    map (foldl (\score char -> score * 5 + valueOf char) 0) $
      foldl
        ( \acc ac ->
            ( case ac of
                Completion str -> str : acc
                _ -> acc
            )
        )
        []
        $ map autocomplete xs
  where
    valueOf '(' = 1
    valueOf '[' = 2
    valueOf '{' = 3
    valueOf '<' = 4
    valueOf _ = 0

autocomplete :: String -> AutocompleteResult
autocomplete = aux []
  where
    toLeft '}' = '{'
    toLeft ')' = '('
    toLeft ']' = '['
    toLeft '>' = '<'
    toLeft _ = error "Invalid Bracket"

    aux :: [Char] -> String -> AutocompleteResult
    aux stack "" = Completion stack
    aux stack (x : xs)
      | x `elem` ['{', '[', '(', '<'] = aux (x : stack) xs
      | otherwise =
        if null stack || toLeft x /= head stack
          then Invalid x
          else aux (tail stack) xs

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)