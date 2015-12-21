module P15 where

import Data.List

-- Problem 15 --
data Ingredient = I { cal :: Int, cap :: Int, dur :: Int, flav :: Int, tex :: Int }
                deriving (Show)

type Recipe = [(Int, Ingredient)]

ingredients :: [Ingredient]
ingredients = [(I 3 2 0 (-2) 0),
               (I 3 0 5 (-3) 0),
               (I 8 0 0 5 (-1)),
               (I 8 0 (-1) 0 5)]

testIngs = [(I 8 (-1) (-2) 6 3), (I 3 2 3 (-2) (-1))]
testRecipe = zip [44,56] testIngs :: Recipe
testProperties = [cap, dur, flav, tex]

allRecipes :: Int -> [Ingredient] -> [Recipe]
allRecipes n (x:[]) = [[(n,x)]]
allRecipes n (x:xs) = -- attach every num from 0..n to x.
  let h = map (\i -> (i,x)) [0..n]
      ts = concatMap (\head -> map (head:) (allRecipes (n - fst head) xs)) h
  in ts

scoreRecipe r = let
  byProperty = transpose $ map distributeQty r
  summed = map ((max 0) . sum) byProperty
  in product summed

distributeQty (q,i) = map (q *) [cap i, dur i, flav i, tex i]

p15_1 = let
  allRs = allRecipes 100 ingredients
  scores = map scoreRecipe allRs
  best = maximum scores
  in print best

p15_2 = let
  allRs = allRecipes 100 ingredients
  loCal = filter (\r -> recipeCals r == 500) allRs
  scores = map scoreRecipe loCal
  best = maximum scores
  in print best

recipeCals :: Recipe -> Int
recipeCals r = sum $ map (\(q,i) -> q * (cal i)) r



