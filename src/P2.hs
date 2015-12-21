module P2 where

import AdventUtil
import Text.Megaparsec
import Data.List

p2 = do
  gifts <- slurpLinesWith giftParser "2.txt"
  let dims = map allPairs gifts
      part1 = sum $ map (sum . wrap . sort . (map prod)) dims
      volumes = vols gifts
      ps = minPerims dims
      part2 = zipWith (+) volumes ps
  print $ part1
  print $ sum part2

giftParser :: Parsec String [Int]
giftParser = do
  l <- parseInt
  char 'x'
  w <- parseInt
  char 'x'
  h <- parseInt
  return [l,w,h]

allPairs :: [a] -> [(a,a)]
allPairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

prod :: Num a => (a,a) -> a
prod (x,y) = x * y

wrap xs@(x:_) = x : map (2*) xs

vols = map product

minPerims xs = map (minimum . map perimeter) xs


perimeter (x,y) = (2 * x) + (2 * y)



