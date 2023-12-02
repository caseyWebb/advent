{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Foldable (find)
import Data.Maybe (mapMaybe)

data Spot = Open Int | Hit deriving (Eq, Show)

type Board = [[Spot]]

main :: IO ()
main = do
  file <- readFile "input.txt"

  let (numbersCSV : restLines) = lines file

  let numbers = map read $ split (== ',') numbersCSV :: [Int]
  let boards = parseBoards restLines

  let (winningNumber, score) = play boards numbers
  print (winningNumber, score, winningNumber * score)

  let (winningNumber, score) = getLastBoard boards numbers
  print (winningNumber, score, winningNumber * score)

  return ()

play :: [Board] -> [Int] -> (Int, Int)
play _ [] = error "ran out of numbers"
play boards (n : ns) =
  case winner of
    Nothing -> play updatedBoards ns
    Just board -> (n, calculateScore board)
  where
    updatedBoards = map (markBoard n) boards
    winner = find hasBingo updatedBoards

getLastBoard :: [Board] -> [Int] -> (Int, Int)
getLastBoard _ [] = error "ran out of numbers"
getLastBoard boards (n : ns)
  | length updatedBoards == 1 = play updatedBoards ns
  | otherwise = getLastBoard updatedBoards ns
  where
    updatedBoards = filter (not . hasBingo) $ map (markBoard n) boards

calculateScore :: Board -> Int
calculateScore board = sum $ mapMaybe (\case Hit -> Nothing; Open a -> Just a) $ concat board

hasBingo :: Board -> Bool
hasBingo board =
  iter 0
  where
    check = all (\case Hit -> True; Open _ -> False)
    column i = check $ map (!! i) board
    row i = check $ board !! i
    iter i
      | i > 4 = False
      | otherwise = column i || row i || iter (i + 1)

markBoard :: Int -> Board -> Board
markBoard number =
  map
    ( map
        (\case Hit -> Hit; Open n -> if n == number then Hit else Open n)
    )

parseBoards :: [String] -> [Board]
parseBoards lines = aux $ filter (/= []) $ map (map (Open . read) . (filter (/= "") . split (== ' '))) lines
  where
    aux [] = []
    aux rows = take 5 rows : aux (drop 5 rows)

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f str = start : split f remain
  where
    (start, rest) = break f str
    (_, remain) = span f rest