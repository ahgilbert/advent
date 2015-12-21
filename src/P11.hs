module P11 where

import Data.Char
import Data.List
import P5

-- Problem 11 --
p11 = do
  let input = reverse "cqjxjnds" -- work with backwards strings because singly linked lists
      pwStream = filter isValidPassword $ iterate nextString input
  print $ map reverse $ take 2 pwStream

isValidPassword :: String -> Bool
isValidPassword s = hasStreak s && noBlacklist s && twoPair s

-- find a descending streak, because we're looking at backwards strings
hasStreak (a:b:c:ds)
  | (ord a) - 1 == (ord b) && (ord b) - 1 == (ord c) = True
  | True = hasStreak (b:c:ds)
hasStreak _ = False

noBlacklist = noVeto ["i", "o", "l"]

twoPair s = 2 <= (length . nub $ findPairs s)

findPairs (a:b:cs)
  | a == b = (a:b:[]) : findPairs cs
  | True = findPairs (b:cs)
findPairs _ = []

nextString :: String -> String
nextString ('z':[]) = "aa"
nextString ('z':cs) = 'a' : nextString cs
nextString (c:cs) = (chr ((ord c) + 1)) : cs



