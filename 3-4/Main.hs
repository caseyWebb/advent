module Main where

data Move = Forward Int | Up Int | Down Int

main :: IO ()
main = do
  file <- readFile "input.txt"

  let (x, y, _) = foldl move (0, 0, 0) $ map parse (lines file)

  print (x, y)
  print (x * y)

  return ()

move :: (Int, Int, Int) -> Move -> (Int, Int, Int)
move (x, y, aim) move =
  case move of
    Forward n -> (x + n, y + (aim * n), aim)
    Down n -> (x, y, aim + n)
    Up n -> (x, y, aim - n)

parse :: String -> Move
parse ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : nStr) = Forward (read nStr)
parse ('d' : 'o' : 'w' : 'n' : ' ' : nStr) = Down (read nStr)
parse ('u' : 'p' : ' ' : nStr) = Up (read nStr)
parse _ = error "Invalid Input"