module Main where

import Data.Function (on)
import Data.List (group, maximumBy, minimumBy, sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main =
  do
    contents <- readFile "input.txt"

    let (template, rules) = parseInput contents

    print $ solve template rules
    -- print $ expandPolymer template rules 20

    return ()

parseInput :: String -> (String, Map.Map (Char, Char) Char)
parseInput string = (template, rules)
  where
    parseRule :: String -> ((Char, Char), Char)
    parseRule (l : r : _ : _ : _ : _ : c : _) = ((l, r), c)
    parseRule _ = error "Invalid Input"

    (template : empty : ruleStrings) = lines string
    rules = Map.fromList $ map parseRule ruleStrings

solve :: String -> Map.Map (Char, Char) Char -> (Int, Int, Int)
solve template rules = (mostCommon, leastCommon, mostCommon - leastCommon)
  where
    polymer = expandPolymer template rules 10
    mostCommon = maximum . map length . group . sort $ polymer
    leastCommon = minimum . map length . group . sort $ polymer

expandPolymer :: String -> Map.Map (Char, Char) Char -> Int -> String
expandPolymer template rules n = foldl1 (\acc (_ : tail) -> acc ++ tail) $ map (expandPair rules n) $ getPairs template

expandPair :: Map.Map (Char, Char) Char -> Int -> (Char, Char) -> String
expandPair _ 0 _ = ""
expandPair rules n (l, r) = [l] ++ inner n (l, r) ++ [r]
  where
    inner :: Int -> (Char, Char) -> String
    inner 0 _ = ""
    inner n (l, r) = inner (n - 1) (l, c) ++ [c] ++ inner (n - 1) (c, r)
      where
        c = fromJust $ Map.lookup (l, r) rules

getPairs :: String -> [(Char, Char)]
getPairs (a : b : cs) = (a, b) : getPairs (b : cs)
getPairs _ = []
