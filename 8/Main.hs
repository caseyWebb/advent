module Main where

import Data.Bifunctor
import Data.List (find, intersect, length, sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main = do
  --   contents <- readFile "input.test.txt"
  contents <- readFile "input.txt"

  let input = map parseLine $ lines contents

  --   print $ solvePartOne input

  print $ solvePartTwo input

  return ()

parseLine :: String -> ([String], [String])
parseLine line =
  let patterns : output : _ = split (== "|") $ split (== ' ') line
   in (patterns, output)

solvePartOne :: [([String], [String])] -> Int
solvePartOne xs = length $ filter (\output -> length output `elem` [2, 4, 3, 7]) $ concatMap snd xs

solvePartTwo :: [([String], [String])] -> Int
solvePartTwo xs = sum $ map decode xs

decode :: ([String], [[Char]]) -> Int
decode (patterns, output) = digitsToDecimal $ map decipherDigit output
  where
    cipher = getCipher patterns
    decipherDigit pattern = fromJust $ Map.lookup (sort pattern) cipher
    digitsToDecimal = foldl (\acc x -> acc * 10 + x) 0

getCipher :: [String] -> Map.Map String Int
getCipher patterns =
  Map.fromList $
    map
      (Data.Bifunctor.first sort)
      [ (num1Pattern, 1),
        (num2Pattern, 2),
        (num3Pattern, 3),
        (num4Pattern, 4),
        (num5Pattern, 5),
        (num6Pattern, 6),
        (num7Pattern, 7),
        (num8Pattern, 8),
        (num9Pattern, 9),
        (num0Pattern, 0)
      ]
  where
    patternWithLength n = head $ patternsWithLength n
    patternsWithLength n = filter ((== n) . length) patterns

    deducePattern :: Int -> [(String, Int)] -> String
    deducePattern l intersections =
      head $
        filter
          ( \pattern ->
              all
                ( \(intersectedPattern, numIntersections) ->
                    length (intersectedPattern `intersect` pattern) == numIntersections
                )
                intersections
          )
          (patternsWithLength l)

    num1Pattern = patternWithLength 2
    num4Pattern = patternWithLength 4
    num7Pattern = patternWithLength 3
    num8Pattern = patternWithLength 7

    num0Pattern = deducePattern 6 [(num1Pattern, 2), (num4Pattern, 3)]
    num2Pattern = deducePattern 5 [(num1Pattern, 1), (num4Pattern, 2)]
    num3Pattern = deducePattern 5 [(num1Pattern, 2)]
    num5Pattern = deducePattern 5 [(num1Pattern, 1), (num4Pattern, 3)]
    num6Pattern = deducePattern 6 [(num1Pattern, 1)]
    num9Pattern = deducePattern 6 [(num1Pattern, 2), (num3Pattern, 5)]

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f str = start : split f remain
  where
    (start, rest) = break f str
    (_, remain) = span f rest