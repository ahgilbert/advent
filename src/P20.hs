module P20 where

import Data.List
import P17

--target = 34000000

p20 target = do
  let factors = map factorize [1..]
      qualifying = dropWhile (\fs -> sum fs < target) factors
    in print $ head qualifying

factorize n =
  nub $ [1, n] ++ factorize' n 2

factorize' target current
  | current * current > target = []
  | True = let
    (q,r) = quotRem target current
    in if r == 0
       then [current, q] ++ factorize' target (current + 1)
       else factorize' target (current + 1)

p20_2 target = do
  let factors = map factorize2 [1..]
      qualifying = dropWhile (\fs -> sum (map (* 11) fs) < target) factors
    in print $ head qualifying

factorize2 n =
  nub $ dropWhile (\x -> x * 50 < n) $ sort $ factorize' n 1

{-
  map faith [1..]
  faith n = concatMap id $ map (replicate 50) (replicate (n - 1) 0 ++ [n])
-}

houses :: Int -> [(Int, Int)]
houses lastElf = zip [1..] $
                 map sum $
                 transpose $
                 take (min lastElf 1000) elves


elves = map concat $
        map (replicate 50) $
        map (\n -> replicate (n - 1) 0 ++ [n * 11]) $
        [1..]
