module P3 where

import Data.List

-- Problem 3 --

p3_1 = do
  input <- readFile "data/3.txt"
  let houses = scanl followDirections (0,0) input
      uniqHouses = nub houses
    in print $ length uniqHouses

followDirections (x,y) '^' = (x,y + 1)
followDirections (x,y) '>' = (x + 1,y)
followDirections (x,y) 'v' = (x,y - 1)
followDirections (x,y) '<' = (x - 1,y)

p3_2 = do
  input <- readFile "data/3.txt"
  let aHouses = scanl followDirections (0,0) (evens input)
      bHouses = scanl followDirections (0,0) (odds input)
    in print $ length (nub $ aHouses ++ bHouses)

evens (_:x:xs) = x : evens xs
evens _ = []

odds (x:_:xs) = x : odds xs
odds (x:[]) = [x]
odds _ = []


