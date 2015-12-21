module P4 where

-- Problem 4 --

p4_1 = do
  let allHashes = map (cathash "bgvyzdsv") [1..]
      misses = takeWhile (\h -> take 5 h /= "00000") allHashes
  print $ length misses

p4_2 = print "similar"

cathash key = undefined -- md5s . Str . (key ++) . show
