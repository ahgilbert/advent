module P18 where

import AdventUtil
import Data.Array.IO

type LifeArray = IOArray (Int, Int) Bool

bounds :: (Int, Int)
bounds = (0,99)

idxes = [fst bounds .. snd bounds]

p18 = do
  arr <- createP18Array
  val <- liveNeighbors arr (10,50)
  print val

createP18Array = do
  initState <- lines <$> slurp "18.txt"
  arr <- newArray ((fst bounds,fst bounds), (snd bounds, snd bounds)) False :: IO LifeArray
  initializeArray arr $ zip [0..] initState
  return arr

rowFate :: LifeArray -> Int -> IO [Bool]
rowFate arr r = mapM (\c -> cellFate arr (r,c)) idxes

cellFate :: LifeArray -> (Int,Int) -> IO Bool
cellFate arr (x,y) = do
  alreadyAlive <- readArray arr (x,y)
  density <- liveNeighbors arr (x,y)
  return $ if alreadyAlive
           then density == 2 || density == 3
           else density == 3

liveNeighbors :: LifeArray -> (Int, Int) -> IO Int
liveNeighbors arr (x,y) = do
  let faith = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
  length <$> filter id <$> mapM (arrVal arr) faith

arrVal :: LifeArray -> (Int, Int) -> IO Bool
arrVal arr (x,y) =
  if (isBetween bounds x && isBetween bounds y)
  then readArray arr (x,y)
  else return False

isBetween :: (Int, Int) -> Int -> Bool
isBetween (l,h) t = l <= t && t <= h

initializeArray arr is = mapM_ (initializeRow arr) is

initializeRow :: IOArray (Int, Int) Bool -> (Int,[Char]) -> IO ()
initializeRow arr (r,cs) =
  mapM_ (\(c,v) -> writeArray arr (r,c) (if v == '#' then True else False)) (zip [0..] cs)
