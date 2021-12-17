module Main where

data Move = Forward Int | Up Int | Down Int

main :: IO ()
main = do
  file <- readFile "input.txt"

  let (x, y) = foldl move (0, 0) $ map parse (lines file)

  print (x, y)
  print (x * y)

  return ()

move :: (Int, Int) -> Move -> (Int, Int)
move (x, y) move =
  case move of
    Forward n -> (x + n, y)
    Down n -> (x, y + n)
    Up n -> (x, y - n)

parse :: String -> Move
parse ('f' : 'o' : 'r' : 'w' : 'a' : 'r' : 'd' : ' ' : nStr) = Forward (read nStr)
parse ('d' : 'o' : 'w' : 'n' : ' ' : nStr) = Down (read nStr)
parse ('u' : 'p' : ' ' : nStr) = Up (read nStr)
parse _ = error "Invalid Input"