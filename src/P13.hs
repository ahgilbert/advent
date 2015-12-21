module P13 where

import AdventUtil
import Data.Array.IO
import Data.List
import Text.Megaparsec
import P9

-- Problem 13, aka "Problem 9 eats its tail" --

data FriendQuotient = FQ { guestA :: String, guestB :: String, affinity :: Int }
              deriving Show
data SeatingChart = SeatingChart { chart :: [Location], joy :: [(Int,Int)] }
          deriving Show

instance Ord SeatingChart where
  compare a b = compare (joySum (joy a)) (joySum (joy b))
instance Eq SeatingChart where
  a == b = joy a == joy b && ((map name) . chart) a == ((map name) . chart) b

joySum js = sum $ map (\(a,b) -> a + b) js

p13_1 = do
  best <- p13
  print $ scoreChart best

p13_2 = do
  best <- p13
  let slots = map (\(a,b) -> a + b) (joy best)
  print $ sum slots - minimum slots

p13 = do
  (guests, arr) <- slurp13 "13.txt"
  harmonies <- mapM (getChartJoy arr) (permutations guests)
  let best = maximum harmonies
  return best

inject :: Location -> [Location] -> Int -> [Location]
inject name list idx =
  take idx list ++ [name] ++ drop idx list

slurp13 :: String -> IO ([Location], IOArray Int Int)
slurp13 filename = do
  fqs <- slurpLinesWith parseFQ filename
  let guestNames = sort . nub . concatMap (\d -> [guestA d, guestB d]) $ fqs
      locs = zipWith (\n i -> Location n i) guestNames [0..]
      numPairs = (length locs) ^ 2 - 1
  arr <- newArray (0, numPairs) 0
  mapM_ (setFQs arr locs) fqs
  return (locs, arr)

setFQs :: IOArray Int Int -> [Location] -> FriendQuotient -> IO ()
setFQs arr keyset (FQ a b fq) = do
  let n = length keyset
      idxer = guestNum keyset
      flattr = coordFlatten n
  writeArray arr (flattr (idxer a) (idxer b)) fq

guestNum :: [Location] -> String -> Int
guestNum locs target = idx . head $ filter (\(Location n i) -> n == target) locs

parseFQ :: Parsec String FriendQuotient
parseFQ = do
  guestA <- some letterChar
  string " would "
  fq <- parseGainLoss
  string " happiness units by sitting next to "
  guestB <- some letterChar
  return $ FQ guestA guestB fq

parseGainLoss = do
  sign <- choice [string "gain", string "lose"]
  space
  amp <- parseInt
  return $ if (sign == "gain")
           then amp
           else (-1) * amp

getChartJoy :: IOArray Int Int -> [Location] -> IO SeatingChart
getChartJoy arr locs = do
  let hops = (last locs, head locs) : zip locs (tail locs)
      n = length locs
      idxer = coordFlatten n
  fqs <- mapM (getFQs arr idxer) hops
  return (SeatingChart locs fqs)

scoreChart :: SeatingChart -> Int
scoreChart c = sum $ map (\(a,b) -> a + b) (joy c)

getFQs :: IOArray Int Int -> (Int -> Int -> Int) -> (Location, Location) -> IO (Int, Int)
getFQs arr idxer (a,b) = do
  atob <- readArray arr $ idxer (idx a) (idx b)
  btoa <- readArray arr $ idxer (idx b) (idx a)
  return $ (atob, btoa)

