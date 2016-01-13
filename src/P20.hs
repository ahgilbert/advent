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

factorize2 n =
  nub $ factorize2' n 1

factorize2' target current
  | current * current > target = []
  | True = let
    (q,r) = quotRem target current
    in undefined
