module P15 where

p15 = do
  undefined

allCombos :: Int -> [a] -> [(Int, a)]
allCombos 0 xs = map (0,) xs
allCombos _ [] = []
allCombos n (a:[]) = [(n,a)]
allCombos n (a:as) = let faith = zip [0..n] (repeat a)
                         hope = map (\(b,c) -> allCombos (n - b)) as
                     in (0,[])
