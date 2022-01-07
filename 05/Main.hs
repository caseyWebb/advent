{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Foldable (foldlM)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text

type Point = (Int, Int)

type Vector = (Point, Point)

type Line = [Point]

main :: IO ()
main = do
  test <- readFile "input.test.txt"
  actual <- readFile "input.txt"

  print $ solve $ map parseVector $ lines test
  print $ solve $ map parseVector $ lines actual

  return ()

solve :: [Vector] -> Int
solve vectors = fromJust $ foldlM (\acc v -> if v > 1 then Just (acc + 1) else Just acc) 0 $ aux Map.empty vectors
  where
    aux counts [] = counts
    aux counts (v : vs) = aux (incPointCounts counts $ getPoints v) vs

getPoints :: Vector -> Line
getPoints ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, min y1 y2 + i) | i <- [0 .. len]]
  | y1 == y2 = [(min x1 x2 + i, y1) | i <- [0 .. len]]
  | slope > 0 = [(min x1 x2 + i, min y1 y2 + i) | i <- [0 .. len]]
  | otherwise = [(min x1 x2 + i, max y1 y2 - i) | i <- [0 .. len]]
  where
    len = maximum $ map abs [x1 - x2, y1 - y2]
    slope = (y2 - y1) `div` (x2 - x1)

incPointCounts :: Map.Map Point Int -> [Point] -> Map.Map Point Int
incPointCounts =
  foldl
    ( flip
        ( Map.alter
            ( \case
                Nothing -> Just 1
                Just a -> Just (a + 1)
            )
        )
    )

parseVector :: String -> Vector
parseVector str =
  ((x1, y1), (x2, y2))
  where
    (p1Str : p2Str : _) = Text.splitOn (Text.pack " -> ") (Text.pack str)
    (p1 : p2 : _) = map (Text.splitOn (Text.pack ",")) [p1Str, p2Str]
    [[x1, y1], [x2, y2]] = map (map (read . Text.unpack)) [p1, p2] :: [[Int]]

-- This implementation checks every space and while more readable, very slow
--
-- solve :: [Vector] -> Int
-- solve vectors =
--   length [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY], checkPoint vectors (x, y) > 1]
--   where
--     (maxX, maxY) = getSize vectors

-- getSize :: [Vector] -> (Int, Int)
-- getSize vectors =
--   foldl (\(maxX, maxY) (x, y) -> (max maxX x, max maxY y)) (0, 0) coords
--   where
--     coords = foldl (\acc (a, b) -> a : b : acc) [] vectors

-- checkPoint :: [Vector] -> Coord -> Int
-- checkPoint vectors point = foldl (\acc vector -> acc + (if intersects vector point then 1 else 0)) 0 vectors

-- intersects :: Vector -> Coord -> Bool
-- intersects ((x1, y1), (x2, y2)) (px, py)
--   | x1 == x2 && x1 == px = inRange (y1, y2) py
--   | y1 == y2 && y1 == py = inRange (x1, x2) px
--   | otherwise = False

-- inRange :: (Ord a, Enum a) => (a, a) -> a -> Bool
-- inRange (a, b) x = x `elem` [(min a b) .. (max a b)]