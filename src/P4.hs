module P4 where

import Data.Hash.MD5

p4 = do
  let allHashes = zip [1..] $ map (cathash "bgvyzdsv") [1..]
      p1 = dropWhile (\(i,h) -> take 5 h /= "00000") allHashes
      p2 = dropWhile (\(i,h) -> take 6 h /= "000000") p1
  print $ head p1
  print $ head p2

cathash key = md5s . Str . (key ++) . show
