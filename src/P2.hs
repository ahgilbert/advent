module P2 where

import AdventUtil
import Text.Megaparsec
import Data.List

-- Problem 2 --
p2_1 = do
  source <- readFile "data/2.txt"
  let gifts = runParser (some giftParser) "" source
      faith = either (\_ -> []) (map allPairs) gifts
      hope = map (sum . wrap . sort . (map prod)) faith
  print $ sum hope

giftParser :: Parsec String [Int]
giftParser = do
  l <- parseInt
  char 'x'
  w <- parseInt
  char 'x'
  h <- parseInt
  newline
  return [l,w,h]

allPairs :: [a] -> [(a,a)]
allPairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

prod :: Num a => (a,a) -> a
prod (x,y) = x * y

wrap xs@(x:_) = x : map (2*) xs

perimeter (x,y) = (2 * x) + (2 * y)



