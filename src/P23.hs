module P23 where

import AdventUtil
import Text.Megaparsec

p23 = do
  instructions <- slurpLinesWith parseInstruction "23.txt"
  print "ahg"


parseInstruction = undefined
