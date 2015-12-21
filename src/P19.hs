module P19 where

import AdventUtil

-- Problem 19 --
p19 = do
  inp <- slurp "19b.txt"
  let (swaps, seed) = head $ rights [runParser parseMedicine "" inp]
  print seed

parseMedicine :: Parsec String ([(String, String)], String)
parseMedicine = do
  swaps <- some parseReplacement
  newline
  seed <- some letterChar
  return (swaps, seed)

parseReplacement = do
  from <- some letterChar
  string " => "
  to <- some letterChar
  newline
  return (from, to)
