module P9 where

import AdventUtil
import Data.Array.IO
import Data.List
import Text.Megaparsec
import Control.Monad.State

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
  ((shortest, longest), _) <- runStateT (p9' keys) arr
  putStrLn "shortest:"
  print shortest
  putStrLn ""
  putStrLn "longest:"
  print longest

p9' keys = do
  distances <- mapM getTripLength (permutations keys)
  let shortest = minimum distances
      longest = maximum distances
    in return (shortest, longest)

slurp9 :: IO ([Location], IOArray (Int,Int) Int)
slurp9 = do
  distances <- slurpLinesWith parseDistance "9.txt"
  let locNames = sort . nub . concatMap (\d -> [pointA d, pointB d]) $ distances
      locs = zipWith (\n i -> Location n i) locNames [0..]
      n = length locs
  arr <- newArray ((0,0), (n,n)) 0
  mapM_ (setDistances arr locs) distances
  return (locs, arr)

setDistances :: IOArray (Int,Int) Int -> [Location] -> Distance -> IO ()
setDistances arr keyset (Distance a b d) = do
  let idxA = cityNum keyset a
      idxB = cityNum keyset b
  writeArray arr (idxA, idxB) d
  writeArray arr (idxB, idxA) d

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

getTripLength :: [Location] -> StateT (IOArray (Int,Int) Int) IO Trip
getTripLength locs = do
  arr <- get
  let hops = zip locs (tail locs)
      n = length locs
  dists <- mapM getDistance hops
  return (Trip (map name locs) (sum dists))

getDistance :: (Location, Location) -> StateT (IOArray (Int, Int) Int) IO Int
getDistance (a,b) = do
  arr <- get
  liftIO $ readArray arr (idx a, idx b)
