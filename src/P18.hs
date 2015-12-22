module P18 where

import AdventUtil
import Data.Array.IO

type LifeArray = IOArray (Int, Int) Bool

bounds :: (Int, Int)
bounds = (0,99)

idxes = [fst bounds .. snd bounds]

p18 = do
  arr <- createP18Array
  let gens = repeat (getNextGen arr)
  sequence_ (take 100 gens)
  return arr

createP18Array = do
  initState <- lines <$> slurp "18.txt"
  arr <- newArray ((fst bounds,fst bounds), (snd bounds, snd bounds)) False :: IO LifeArray
  initializeArray arr $ zip [0..] (translateInput initState)
  return arr
  where
    translateInput grid = map (map (\v -> if v == '#' then True else False)) grid

countLiveCells :: LifeArray -> IO Int
countLiveCells arr = do
  cells <- mapM (readArray arr) [(x,y) | x <- idxes, y <- idxes]
  return $ length (filter id cells)

getNextGen :: LifeArray -> IO ()
getNextGen arr = do
  next <- mapM (rowFate arr) idxes
  initializeArray arr (zip [0..] next)
  mapM_ (\p -> writeArray arr p True) [(0,0),(0,99),(99,0),(99,99)]

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

initializeRow :: LifeArray -> (Int,[Bool]) -> IO ()
initializeRow arr (r,cs) =
  mapM_ (\(c,v) -> writeArray arr (r,c) v) (zip [0..] cs)
