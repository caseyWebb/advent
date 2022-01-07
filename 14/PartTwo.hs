{-# LANGUAGE TupleSections #-}

module Main where

import Data.Function (on)
import Data.List (group, maximumBy, minimumBy, sort)
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import Data.Maybe (fromJust)

type Pair = (Char, Char)

type RuleMap = Map.Map Pair Char

type PairCount = Map.Map Pair Integer

type LetterCount = Map.Map Char Integer

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (template, rules) = parseInput contents
  let initLetterMap = foldl (Map.unionWith (+)) Map.empty $ map (`Map.singleton` 1) template
  let initPairMap = Map.fromList $ zipWith (curry (,1)) template $ tail template
  let lastLetter = Map.singleton (last template) 1
  let (pairs, letters) = iterate (nextGen rules) (initPairMap, initLetterMap) !! 10

  -- print template
  -- print rules

  let freqs = map snd $ Map.toList letters
  let max = maximum freqs
  let min = minimum freqs

  -- print letters

  print freqs

  print max
  print min
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

nextGen :: RuleMap -> (PairCount, LetterCount) -> (PairCount, LetterCount)
nextGen rules (pairs, letters) = (newPairs, newLetters)
  where
    additionalLetters = map (\(p, c) -> Map.singleton (rules ! p) c) $ Map.toList pairs
    newLetters = foldl (Map.unionWith (+)) letters additionalLetters
    newPairs =
      foldl (Map.unionWith (+)) Map.empty $
        concatMap
          ( \((l, r), n) ->
              map (`Map.singleton` n) $ let c = rules ! (l, r) in [(l, c), (c, r)]
          )
          $ Map.toList pairs