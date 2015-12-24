module P24 where

import AdventUtil

p24 = do
  groups <- slurpLinesWith parseInt "24.txt"
  undefined

-- break all input into equally weighted groups
-- select the grouping with the fewest items in the lightest group
-- report the product of the weights of the lightest group
