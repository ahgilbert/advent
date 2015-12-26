module P25 where

inpRow = 3010 :: Int
inpCol = 3019 :: Int
seed   = 20151125 :: Int
prod   = 252533 :: Int
remBy  = 33554393 :: Int

-- 1,1 = 1
-- 2,2 = 5
-- 5,3 = 26
-- x,y = 
-- 26305181 too high, 5087747 too low

cantorRC :: Int -> Int -> Int
cantorRC r c = let
  rowStart = 1 + (sum [0..r - 1])
  colModifier = sum $ take (c - 1) [r + 1..]
  in rowStart + colModifier

next25 x = rem (x * prod) remBy

p25 = let
  idx = cantorRC inpRow inpCol
  allPWs = zip [0..] $ iterate next25 seed
  in print $ allPWs !! idx

test25 r c = let
  idx = cantorRC r c
  allPWs = iterate next25 seed
  in print $ allPWs !! (idx - 1)

testCantorRC r =
  map (cantorRC r) [1..6]
