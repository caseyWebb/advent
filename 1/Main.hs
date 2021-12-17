module Main where

import System.IO

main :: IO ()
main = do
  file <- readFile "input.txt"

  print $ solve $ map read (lines file)

  return ()

solve :: [Int] -> Int
solve xs = aux xs 0
  where
    aux (x1 : x2 : xs) count = aux (x2 : xs) (count + (if x1 <= x2 then 1 else 0))
    aux _ count = count