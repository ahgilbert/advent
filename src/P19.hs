module P19 where

import AdventUtil
import Data.Either
import Data.List
import Data.Tree
import Text.Megaparsec

-- Problem 19 --
slurp19 = do
  inp <- slurp "19.txt"
  let (swaps, seed) = head $ rights [runParser parseMedicine "" inp]
      swaps' = map (\(a,b) -> (b,a)) swaps
  return (swaps', seed)

p19 = do
  (swaps, seed) <- slurp19
  let molecules = nub $ synth swaps 1 "" seed
    in print $ (molecules, length molecules)

groupSwaps swaps = let
  sorted = sortBy (\a b -> compare (fst a) (fst b)) swaps
  grouped = groupBy (\a b -> (fst a) == (fst b)) sorted
  in map (\xs@((k,_):_) -> (k, map snd xs)) grouped

synth :: [(String, String)] -> Int -> String -> String -> [String]
synth _ 0 sofar seed = [sofar ++ seed]
synth _ _ _ "" = []
synth rules tick sofar seed =
  let matches = filter (\rule -> isPrefixOf (fst rule) seed) rules
      continuations = concatMap (\r -> synth
                                       rules
                                       (tick - 1)
                                       (sofar ++ snd r)
                                       (drop (length (fst r)) seed))
                      matches
      skipThis = synth rules tick (sofar ++ [head seed]) (drop 1 seed)
  in continuations ++ skipThis

p19_2 = do
  (swaps, seed) <- slurp19
  let faith = iterate (p19Cycle swaps) [seed]
      hope = span (\xs -> minimum (map length xs) > 1) faith
      passes = length $ fst hope
      charity = snd hope
  print passes
  print $ head charity

-- run synth, yield the 10 most promising results
p19Cycle :: [(String, String)] -> [String] -> [String]
p19Cycle swaps seeds =
  nub $ take 50 $ sortOn length $ concatMap (synth swaps 1 "") seeds

-- Parsers --

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
