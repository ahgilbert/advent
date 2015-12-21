module P17 where

import AdventUtil
import Data.List

p17 = do
  scoops <- (reverse . sort) <$> slurpLinesWith parseInt "17.txt"
  return $ getCombosSumming 150 scoops

getCombosSumming target (n:ns)
  | target == n = [[n]] ++ getCombosSumming target ns
  | target < 0 = []
  | True = withN ++ withoutN
  where
    withN = map (n:) (getCombosSumming (target -n) ns)
    withoutN = getCombosSumming target ns
getCombosSumming _ [] = []
