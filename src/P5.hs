module P5 where

import Data.List

-- Problem 5 --
p5 isNice = do
  input <- lines <$> readFile "data/5.txt"
  let faith = filter isNice input
  print $ length faith
  --  mapM_ (putStrLn . show) faith

p5_1 = p5 isNice1

p5_2 = p5 isNice2

isNice1 :: String -> Bool
isNice1 s = and [hasVowels s, hasDubs s, noVeto ["ab", "cd", "pq", "xy"] s]

hasVowels s = length (filter (\c -> elem c "aeiou") s) >= 3

hasDubs s = any dubs $ zip s (tail s)
  where dubs (x,y) = x == y

noVeto blackList s = not . or $ map (flip isInfixOf s) blackList

isNice2 s = and [dupes s, aba s]

dupes (a:b:cs) = (a:b:[]) `isInfixOf` cs || dupes (b:cs)
dupes _ = False

aba (a:b:c:ds)
  | a == c = True
  | True = aba (b:c:ds)
aba _ = False
