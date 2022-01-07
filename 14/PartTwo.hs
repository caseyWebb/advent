module Main where

import Data.Function (on)
import Data.List (group, maximumBy, minimumBy, sort)
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import Data.Maybe (fromJust)

type Pair = (Char, Char)

type RuleMap = Map.Map Pair Char

type PairCount = Map.Map Pair Int

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (template, rules) = parseInput contents
  let initPairMap = sumPairCount $ zipWith (\a b -> Map.singleton (a, b) 1) template $ tail template
  let lastLetter = Map.singleton (last template) 1
  let pairs = iterate (step rules) initPairMap !! 40

  let freq =
        map snd $
          Map.toList $
            foldl (Map.unionWith (+)) lastLetter $
              map (\((a, _), b) -> Map.singleton a b) $
                Map.toList pairs

  let max = maximum freq
  let min = minimum freq

  print $ max - min

  return ()

parseRule :: String -> RuleMap
parseRule (l : r : _ : _ : _ : _ : c : _) = Map.singleton (l, r) c
parseRule _ = error "Invalid Input"

parseInput :: String -> (String, RuleMap)
parseInput string = (template, rules)
  where
    (template : empty : ruleStrings) = lines string
    rules = foldl Map.union Map.empty $ map parseRule ruleStrings

sumPairCount :: [PairCount] -> PairCount
sumPairCount = foldl (Map.unionWith (+)) Map.empty

step :: RuleMap -> PairCount -> PairCount
step rules pairs =
  sumPairCount $
    concatMap
      ( \((l, r), n) ->
          map (`Map.singleton` n) $ let c = rules ! (l, r) in [(l, c), (c, r)]
      )
      $ Map.toList pairs