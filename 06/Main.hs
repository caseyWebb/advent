module Main where

import qualified Data.Map as Map
import Data.Maybe (fromJust)

main :: IO ()
main = do
  -- csv <- readFile "input.test.txt"
  csv <- readFile "input.txt"

  let fish = map read $ split (== ',') csv :: [Int]

  print $ solve 256 fish

  return ()

solve :: Int -> [Int] -> Int
solve i fish = sum $ aux counts days
  where
    days = take i $ cycle [0, 1, 2, 3, 4, 5, 6, 7, 8]
    counts = foldr (Map.alter (Just . (+) 1 . fromJust)) (Map.fromList [(x, 0) | x <- [0 .. 8]]) fish
    aux =
      foldl
        ( \map x ->
            Map.alter
              (Just . (+) (fromJust $ Map.lookup x map) . fromJust)
              ((x + 7) `mod` 9)
              map
        )

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f str = start : split f remain
  where
    (start, rest) = break f str
    (_, remain) = span f rest