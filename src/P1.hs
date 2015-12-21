module P1 where

-- Problem 1 --
p1_1 = do
  instructions <- readFile "data/1.txt"
  let up = length $ filter (== '(') instructions
      down = length $ filter (== ')') instructions
   in putStrLn . show $ up - down

p1_2 = do
  instructions <- readFile "data/1.txt"
  let faith = sumBraces instructions
      hope = length $ takeWhile (0 <=) faith
  print hope

sumBraces :: String -> [Int]
sumBraces instructions = scanl (\s c -> if c == '(' then s + 1 else s - 1) 0 instructions
