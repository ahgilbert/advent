module P8 where

import Text.Megaparsec
import Data.Either

  -- Problem 8 --
p8_1 = do
  input <- lines <$> readFile "data/8.txt"
  let mem = sum $ map sum $ rights $ map (runParser memSize "") input
      code = sum $ map codeSize input
  print code
  print mem
  print $ code - mem

codeSize = length

memSize = do
  char '\"'
  count <- option [0] $ some $ choice [try escapedChar, noneOf "\"" >> return 1]
  char '\"'
  return count

escapedChar :: Parsec String Int
escapedChar = do
  (char '\\')
  (choice [try (char '\\'),
           try (char '\"'),
           try (char 'x' >> hexDigitChar >> hexDigitChar)])
  return 1



p8_2 = do
  input <- lines <$> readFile "data/8.txt"
  let raw = map length input
      encoded = map (length . show) input
  print $ sum $ zipWith (-) encoded raw

