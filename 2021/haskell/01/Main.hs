module Main where

import System.IO

main :: IO ()
main = do
  file <- readFile "input.txt"

  -- print $ solve $ map read (lines file)

  print $ solve $ calculateWindows $ map read (lines file)

  print $ solve' $ map read (lines file)

  return ()

solve :: [Int] -> Int
solve xs = aux xs 0
  where
    aux (x1 : x2 : xs) count = aux (x2 : xs) (count + (if x1 < x2 then 1 else 0))
    aux _ count = count

calculateWindows :: [Int] -> [Int]
calculateWindows (x1 : x2 : x3 : xs) = (x1 + x2 + x3) : calculateWindows (x2 : x3 : xs)
calculateWindows _ = []

solve' :: [Int] -> Int
solve' xs = aux xs 0
  where
    aux (x1 : x2 : x3 : x4 : xs) count =
      aux
        (x2 : x3 : x4 : xs)
        ( count
            + ( if x1 + x2 + x3 < x2 + x3 + x4
                  then 1
                  else 0
              )
        )
    aux _ count = count