module P10 where

-- Problem 10 --
p10_1 = do
  let input = "1113122113"
  print $ take 51 $ map length $ iterate lookSay input

lookSay :: String -> String
lookSay s =
  let (inits, rest) = breakDupes s
   in translate inits ++ if rest /= [] then lookSay rest else []

breakDupes s@(a:_) =
  let chunkSize =  length $ takeWhile (==a) s
   in splitAt chunkSize s
breakDupes _ = ([],[])

translate s = show (length s) ++ [head s]




