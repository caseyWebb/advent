module Main where

type BinaryDigits = [Int]

main :: IO ()
main = do
  file <- readFile "input.txt"

  let xs = map toBinaryDigits (lines file)

  print $ "power consumption: " ++ show (getPowerConsumption xs)
  print $ "life support rating: " ++ show (getLifeSupportRating xs)

  return ()

getPowerConsumption :: [BinaryDigits] -> Int
getPowerConsumption xs = product $ map toDecimal [getGamma xs, getEpsilon xs]

getLifeSupportRating :: [BinaryDigits] -> Int
getLifeSupportRating list = product $ map (toDecimal . aux 0 list) [getGamma, getEpsilon]
  where
    aux _ [] _ = error "oops."
    aux _ [x] _ = x
    aux i xs f = aux (i + 1) (filter (\digits -> (digits !! i) == digit) xs) f
      where
        digit = f xs !! i

getGamma :: [BinaryDigits] -> BinaryDigits
getGamma xs = [if x >= 0 then 1 else 0 | x <- tallyDigits xs]

getEpsilon :: [BinaryDigits] -> BinaryDigits
getEpsilon xs = [if x < 0 then 1 else 0 | x <- tallyDigits xs]

tallyDigits :: [BinaryDigits] -> BinaryDigits
tallyDigits xs = foldl1 (zipWith (+)) (map (map (\x -> if x > 0 then 1 else -1)) xs)

toBinaryDigits :: String -> BinaryDigits
toBinaryDigits = map (\digit -> read [digit])

toDecimal :: BinaryDigits -> Int
toDecimal digits = aux (reverse digits) 0
  where
    aux [] _ = 0
    aux (x : xs) e = (x * 2 ^ e) + aux xs (e + 1)