module Main where

import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set

data Fold = HeightWise Int | WidthWise Int deriving (Show)

main :: IO ()
main = do
  contents <- readFile "input.txt"

  let (paper, folds) = parseInput contents

  printResult $ foldl fold paper folds

  return ()

parseInput :: String -> (Set.Set (Maybe (Int, Int)), [Fold])
parseInput str = (points, folds)
  where
    parseFold ('x' : '=' : digits) = WidthWise $ read digits
    parseFold ('y' : '=' : digits) = HeightWise $ read digits
    parseFold _ = error "Invalid Input"

    tuple (a : b : _) = (a, b)
    tuple _ = error "Invalid Input"

    (pointStrs : foldStrs : _) = split (== "") $ lines str
    points = Set.fromList $ map (Just . tuple . map read . split (== ',')) pointStrs
    folds = map (parseFold . (\str -> split (== ' ') str !! 2)) foldStrs

fold :: Set.Set (Maybe (Int, Int)) -> Fold -> Set.Set (Maybe (Int, Int))
fold paper f = foldl (\acc point -> Set.insert (foldPoint point) acc) Set.empty paper
  where
    foldPoint Nothing = Nothing
    foldPoint (Just (x, y)) = case f of
      WidthWise crease -> if x > crease then Just (crease - (x - crease), y) else if x == crease then Nothing else Just (x, y)
      HeightWise crease -> if y > crease then Just (x, crease - (y - crease)) else if y == crease then Nothing else Just (x, y)

printResult :: Set.Set (Maybe (Int, Int)) -> IO ()
printResult set = do
  mapM_ (putStrLn . unwords) grid
  where
    points = catMaybes $ Set.toList set
    w = maximum $ map fst points
    h = maximum $ map snd points
    grid = [[if Set.member (Just (x, y)) set then "#" else " " | x <- [0 .. w]] | y <- [0 .. h]]

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f str = start : split f remain
  where
    (start, rest) = break f str
    (_, remain) = span f rest