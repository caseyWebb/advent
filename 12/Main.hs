module Main where

import Data.Char
import Data.List (find, findIndex, nub, scanl, sort)
import qualified Data.Map as Map
import Data.Maybe

type Graph = Map.Map String [String]

main :: IO ()
main = do
  graph <- getInput "input.txt"
  -- graph <- getInput "input.test.1.txt"

  let paths = sort $ map (foldl1 (\acc x -> acc ++ "," ++ x)) $ getAllPaths graph

  -- mapM_ print paths

  print $ length paths

  return ()

getAllPaths :: Graph -> [[String]]
getAllPaths graph =
  map (map fromJust) $
    filter (\path -> Nothing `notElem` path) $
      traverse [] "start"
  where
    traverse :: [String] -> String -> [[Maybe String]]
    traverse visited "end" = [[Just "end"]]
    traverse prevVisited start
      | null next = [[Nothing]]
      | otherwise =
        map
          ( \tail ->
              Just start : tail
          )
          $ concatMap (traverse visited) next
      where
        visited = start : prevVisited
        visitedSmallCaves = filter (all isLower) visited
        smallCaveVisitedTwice = length visitedSmallCaves > length (nub visitedSmallCaves)
        next = filter (\cave -> all isUpper cave || cave `notElem` visited || not smallCaveVisitedTwice) $ fromJust $ Map.lookup start graph

getInput :: String -> IO Graph
getInput filename = do
  input <- readFile filename
  return (parseGraph input)

parseGraph :: String -> Graph
parseGraph str =
  foldl
    ( \acc (a : b : xs) ->
        Map.insertWith (++) b [a | a /= "start"] $ Map.insertWith (++) a ([b | b /= "start"]) acc
    )
    Map.empty
    $ map (split '-') $ lines str

split :: Char -> String -> [String]
split _ [] = []
split delim str = start : split delim remain
  where
    (start, rest) = break (== delim) str
    (_, remain) = span (== delim) rest

foo :: IO ()
foo = do
  str <- readFile "input.test.0.txt"

  print $ map (split '-') $ lines str

  return ()
