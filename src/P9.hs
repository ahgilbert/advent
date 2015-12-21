module P9 where

import AdventUtil
import Data.Array.IO
import Data.List
import Text.Megaparsec

-- Problem 9 (crying for better state management...) --

data Distance = Distance { pointA :: String, pointB :: String, dist :: Int }
              deriving Show
data Location = Location { name :: String, idx :: Int }
              deriving Show
data Trip = Trip { route :: [String], total :: Int }
          deriving Show

instance Ord Trip where
  compare a b = compare (total a) (total b)
instance Eq Trip where
  a == b = total a == total b && route a == route b

p9 = do
  (keys, arr) <- slurp9
  distances <- mapM (getTripLength arr) (permutations keys)
  let shortest = minimum distances
      longest = maximum distances
  putStrLn "shortest:"
  print shortest
  putStrLn ""
  putStrLn "longest:"
  print longest

slurp9 :: IO ([Location], IOArray Int Int)
slurp9 = do
  distances <- slurpLinesWith parseDistance "9.txt"
  let locNames = sort . nub . concatMap (\d -> [pointA d, pointB d]) $ distances
      locs = zipWith (\n i -> Location n i) locNames [0..]
      numPairs = (length locs) ^ 2 - 1
  arr <- newArray (0, numPairs) 0
  mapM_ (setDistances arr locs) distances
  return (locs, arr)

setDistances :: IOArray Int Int -> [Location] -> Distance -> IO ()
setDistances arr keyset (Distance a b d) = do
  let n = length keyset
      idxer = cityNum keyset
      flattr = coordFlatten n
  writeArray arr (flattr (idxer a) (idxer b)) d
  writeArray arr (flattr (idxer b) (idxer a)) d

coordFlatten :: Int -> Int -> Int -> Int
coordFlatten n r c = (n * r) + c

cityNum :: [Location] -> String -> Int
cityNum locs target = idx . head $ filter (\(Location n i) -> n == target) locs

parseDistance :: Parsec String Distance
parseDistance = do
  pointA <- some letterChar
  string " to "
  pointB <- some letterChar
  string " = "
  distance <- read <$> some digitChar
  return $ Distance pointA pointB distance

getTripLength :: IOArray Int Int -> [Location] -> IO Trip
getTripLength arr locs = do
  let hops = zip locs (tail locs)
      n = length locs
      idxer = coordFlatten n
  dists <- mapM (getDistance arr n) hops
  return (Trip (map name locs) (sum dists))

getDistance :: IOArray Int Int -> Int -> (Location, Location) -> IO Int
getDistance arr n (a,b) = readArray arr (coordFlatten n (idx a) (idx b))
