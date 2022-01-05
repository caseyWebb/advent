module Main where

import qualified Data.Map as Map

main :: IO ()
main = do
  input <- getInput

  print $ solve input 100

  return ()

getTestInput :: IO (Map.Map (Int, Int) Int)
getTestInput = do
  input <- readFile "input.test.txt"
  return (parseMatrix input)

getInput :: IO (Map.Map (Int, Int) Int)
getInput = do
  input <- readFile "input.txt"
  return (parseMatrix input)

parseMatrix :: String -> Map.Map (Int, Int) Int
parseMatrix str =
  Map.fromList [((x, y), read [matrix !! y !! x]) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
  where
    matrix = lines str
    w = length $ head matrix
    h = length matrix

solve :: Map.Map (Int, Int) Int -> Int -> Int
solve matrix n =
  snd $
    foldl
      ( \(acc, count) _ ->
          let (next, flashed) = step (incrementAll acc) [] in (next, count + length flashed)
      )
      (matrix, 0)
      [1 .. n]
  where
    step :: Map.Map (Int, Int) Int -> [(Int, Int)] -> (Map.Map (Int, Int) Int, [(Int, Int)])
    step matrix flashed
      | null flashers = (resetFlashed matrix, flashed)
      | otherwise = step (incrementPoints matrix adjacent) (flashed ++ flashers)
      where
        flashers = filter (`notElem` flashed) $ Map.keys $ Map.filter (> 9) matrix
        adjacent = concatMap (getAdjacent matrix) flashers

incrementAll :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
incrementAll = Map.map (+ 1)

incrementPoints :: Map.Map (Int, Int) Int -> [(Int, Int)] -> Map.Map (Int, Int) Int
incrementPoints = foldl (flip (Map.adjust (+ 1)))

getAdjacent :: Map.Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
getAdjacent matrix (x, y) =
  [ (x', y')
    | x' <- [x - 1 .. x + 1],
      x' <= width matrix,
      x' >= 0,
      y' <- [y - 1 .. y + 1],
      y' <= height matrix,
      y' >= 0,
      (x', y') /= (x, y)
  ]

resetFlashed :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
resetFlashed matrix =
  foldl (\acc point -> Map.insert point 0 acc) matrix $ Map.keys $ Map.filter (> 9) matrix

height :: Map.Map (Int, Int) Int -> Int
height matrix = maximum $ map snd $ Map.keys matrix

width :: Map.Map (Int, Int) Int -> Int
width matrix = maximum $ map fst $ Map.keys matrix
