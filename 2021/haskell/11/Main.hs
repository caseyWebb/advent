module Main where

import Data.List (find, findIndex, scanl)
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
  input <- getInput

  print $ solvePartOne input 100
  print $ solvePartTwo input

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

solvePartOne :: Map.Map (Int, Int) Int -> Int -> Int
solvePartOne matrix n = sum $ map snd $ take (n + 1) $ series matrix

solvePartTwo :: Map.Map (Int, Int) Int -> Int
solvePartTwo matrix = fromJust $ findIndex (\(_, n) -> n == Map.size matrix) $ series matrix

series :: Map.Map (Int, Int) Int -> [(Map.Map (Int, Int) Int, Int)]
series matrix = scanl (\(acc, _) _ -> step (incrementAll acc) []) (matrix, 0) [1 ..]

step :: Map.Map (Int, Int) Int -> [(Int, Int)] -> (Map.Map (Int, Int) Int, Int)
step matrix flashed
  | null flashers = (resetFlashed matrix, length flashed)
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
      x' <= maximum (map fst $ Map.keys matrix),
      x' >= 0,
      y' <- [y - 1 .. y + 1],
      y' <= maximum (map snd $ Map.keys matrix),
      y' >= 0,
      (x', y') /= (x, y)
  ]

resetFlashed :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
resetFlashed matrix =
  foldl (\acc point -> Map.insert point 0 acc) matrix $ Map.keys $ Map.filter (> 9) matrix
