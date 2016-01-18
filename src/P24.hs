module P24 where

import AdventUtil
import Data.List
import P17

slurp24 = do
  packages <- reverse <$> slurpLinesWith parseInt "24.txt"
  let totalWeight = sum packages
      target = totalWeight `div` 4
  return (packages, target)

p24 = do
  (pkgs, target) <- slurp24
  let groups = getCombosSumming target pkgs
      byCount = sortOn length groups
  return (byCount, pkgs)

p24undone all byCount = let -- find some triplet in byCount that contains all elems of all
  passenger = head byCount
  others = killDupes passenger $ tail byCount
  pairs = [(x,y) | (x:xt) <- tails others, y <- xt]
  in pairs

killDupes killer bKilled =
  filter (\k -> not . or $ map (flip elem killer) k) bKilled
  -- given a list of ints, and a list of lists of ints
  -- 

-- calculate total weight / 3
-- break all input into groups weighted as above
-- select the grouping with the fewest items in the lightest group
-- report the product of the weights of the lightest group
