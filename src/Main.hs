module Main where

import AdventUtil
import P1
import P2
import P3
import P4
import P5
import P6
import P7
import P8
import P9
import P10
import P11
import P12
import P13
import P14
import P15
import P16
import P17
import P18
import P19
import P20
import P24
import P25

import Data.List

quickProblems = [p1_1, p1_2, p2, p3_1, p3_2, p5_1, p5_2, p7, p8_1, p8_2, p9, p10, p11]
slowProblems = [p4, p6_1, p18]

main :: IO ()
main = do
  (swaps, seed) <- slurp19
  let faith = iterate (p19Cycle swaps) [seed]
      hope = dropWhile (\xs -> minimum (map length xs) > 10) faith
  mapM_ print $ zip (head hope) (map length (head hope))

-- run synth, yield the 10 most promising results
p19Cycle :: [(String, String)] -> [String] -> [String]
p19Cycle swaps seeds =
  nub $ take 50 $ sortOn length $ concatMap (synth swaps 1 "") seeds
