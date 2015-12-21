module P16 where

import AdventUtil
import Text.Megaparsec

-- Problem 16 --

data Sue = Sue { sue :: Int, facts :: [(String, Int)] }
         deriving (Show)

p16 = do
  sues <- slurpLinesWith parseSue "16.txt"
  let matches = filter sueMatches sues
  mapM print matches

parseSue = do
  string "Sue "
  num <- parseInt
  string ": "
  facts <- some parseSueFact -- separated by commas?
  return $ Sue num facts

parseSueFact = do
  key <- some letterChar
  char ':'
  space
  n <- parseInt
  optional (char ',' >> space)
  return (key, n)

sueMatches :: Sue -> Bool
sueMatches (Sue _ fs) = and $ map checkFact fs

checkFact (targetProp, num) = targ num
  where targ = snd . head $ filter (\(prop,_) -> prop == targetProp) targetFacts

targetFacts = [("children", (==3)),
               ("cats", (>7)),
               ("samoyeds", (==2)),
               ("pomeranians", (<3)),
               ("akitas", (==0)),
               ("vizslas", (==0)),
               ("goldfish", (<5)),
               ("trees", (>3)),
               ("cars", (==2)),
               ("perfumes", (==1))]

