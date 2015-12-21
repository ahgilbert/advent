module P14 where

import AdventUtil
import Data.List
import Text.Megaparsec

-- Problem 14 - Reindeer Olympics --
data Reindeer = R { moniker :: String,
                    moving :: (Int, Int),
                    rest :: Int }
              deriving (Show)

timer = 2503

p14_1 = do
  input <- slurpLinesWith parseReindeer "14.txt"
  let race = map (\r -> (r, getReindeerLoc r timer)) input
      race' = sortBy (\a b -> compare (snd b) (snd a)) race
  mapM print race'

p14_2 = do
  deer <- slurpLinesWith parseReindeer "14.txt"
  let slices = map (getReindeerLocs deer) [1..timer]
      leaders = map scoreTimeslice slices
      points = map (foldl (+) 0) (transpose leaders)
  print $ sort points

scoreTimeslice :: [Int] -> [Int]
scoreTimeslice distances =
  let maxDistance = maximum distances
  in map (\d -> if d == maxDistance then 1 else 0) distances

parseReindeer :: Parsec String Reindeer
parseReindeer = do
  moniker <- some letterChar
  string " can fly "
  speed <- parseInt
  string " km/s for "
  stamina <- parseInt
  string " seconds, but then must rest for "
  rest <- parseInt
  return (R moniker (speed, stamina) rest)

getReindeerLocs :: [Reindeer] -> Int -> [Int]
getReindeerLocs rs clock = map (flip getReindeerLoc clock) rs

getReindeerLoc :: Reindeer -> Int -> Int
getReindeerLoc deer@(R n (speed, stamina) rest) clock
  | clock > (stamina + rest) =
    let burstDist = speed * stamina
        intervals = quot clock (stamina + rest)
        partial = rem clock (stamina + rest)
    in (burstDist * intervals) + (getReindeerLoc deer partial)
  | True = (speed * (min stamina clock))

isFlying :: Reindeer -> Int -> Bool
isFlying r@(R _ (_, stamina) rest) clock =
  if (clock <= stamina)
  then True
  else not $ isResting r (clock - stamina)

isResting :: Reindeer -> Int -> Bool
isResting r@(R _ (_, stamina) rest) clock =
  if (clock <= rest)
  then True
  else not $ isFlying r (clock - rest)
