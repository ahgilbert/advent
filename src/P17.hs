module P17 where

import AdventUtil
import Data.List

p17 = do
  scoops <- (reverse . sort) <$> slurpLinesWith parseInt "17.txt"
  let allCombos = getCombosSumming 150 scoops
      part1 = length allCombos
      minN = minimum $ map length allCombos
      part2 = length $ filter (\x -> length x == minN) allCombos
  print part1
  print part2

getCombosSumming target (n:ns)
  | target == n = [[n]] ++ getCombosSumming target ns
  | target < 0 = []
  | True = withN ++ withoutN
  where
    withN = map (n:) (getCombosSumming (target -n) ns)
    withoutN = getCombosSumming target ns
getCombosSumming _ [] = []
