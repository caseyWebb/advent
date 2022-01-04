module Main where

import Data.List (sort)

main :: IO ()
main = do
  -- contents <- readFile "input.test.txt"
  contents <- readFile "input.txt"

  let matrix = map (map (\c -> read [c])) $ lines contents :: [[Int]]

  print $ solvePartOne matrix
  print $ solvePartTwo matrix

  return ()

solvePartOne :: [[Int]] -> Int
solvePartOne matrix = sum minimums + length minimums
  where
    minimums = map (\(x, y) -> matrix !! y !! x) $ findLocalMinimums matrix

solvePartTwo :: [[Int]] -> Int
solvePartTwo matrix =
  product $ take 3 $ reverse $ sort $ map (length . calculateBasin matrix) $ findLocalMinimums matrix

findLocalMinimums :: [[Int]] -> [(Int, Int)]
findLocalMinimums matrix =
  [ (x, y)
    | x <- [0 .. width matrix - 1],
      y <- [0 .. height matrix - 1],
      isLocalMinimum matrix x y
  ]

isLocalMinimum :: [[Int]] -> Int -> Int -> Bool
isLocalMinimum matrix x y = all (> matrix !! y !! x) [left, right, top, bottom]
  where
    left = if x == 0 then 9 else matrix !! y !! (x - 1)
    right = if x == width matrix - 1 then 9 else matrix !! y !! (x + 1)
    top = if y == 0 then 9 else matrix !! (y - 1) !! x
    bottom = if y == height matrix - 1 then 9 else matrix !! (y + 1) !! x

calculateBasin :: [[Int]] -> (Int, Int) -> [(Int, Int)]
calculateBasin matrix = aux []
  where
    aux points (x, y) =
      foldr (flip aux) (points ++ adjacent) adjacent
      where
        adjacent =
          filter
            ( \(x, y) ->
                x >= 0
                  && y >= 0
                  && x < width matrix
                  && y < height matrix
                  && (x, y) `notElem` points
                  && matrix !! y !! x /= 9
            )
            [ (x - 1, y),
              (x + 1, y),
              (x, y - 1),
              (x, y + 1)
            ]

height :: [[Int]] -> Int
height = length

width :: [[Int]] -> Int
width matrix = length $ head matrix
