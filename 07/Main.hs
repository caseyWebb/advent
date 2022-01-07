module Main where

import Data.List (sort)

main :: IO ()
main = do
  -- csv <- readFile "input.test.txt"
  csv <- readFile "input.txt"

  print $ solve $ map read $ split (== ',') csv

  return ()

solve :: [Int] -> Int
solve crabs = minimum [calculateFuel crabs i | i <- [minimum crabs .. maximum crabs]]

-- part 1
--
-- calculateFuel :: [Int] -> Int -> Int
-- calculateFuel crabs i =
--   i * (length left - length right) + sum right - sum left
--   where
--     orderedCrabs = sort crabs
--     left = filter (< i) orderedCrabs
--     right = filter (> i) orderedCrabs

calculateFuel :: [Int] -> Int -> Int
calculateFuel crabs pos = sum $ map (\c -> let x = abs (pos - c) in x * (x + 1) `div` 2) crabs

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f str = start : split f remain
  where
    (start, rest) = break f str
    (_, remain) = span f rest