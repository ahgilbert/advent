module P23 where

seed23_1 = 20895
seed23_2 = 60975

p23 = do
  print $ length $ takeWhile (/= 1) $ iterate p23iter seed23_1
  print $ length $ takeWhile (/= 1) $ iterate p23iter seed23_2

-- if the number is even triple it
p23iter :: Integer -> Integer
p23iter n
  | mod n 2 == 0 = div n 2
  | True = (n * 3) + 1
