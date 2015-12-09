module Main where

import Text.Megaparsec
import Data.List
import Data.Hash.MD5
import Data.Either

main :: IO ()
main = p6_1

p2_2 = do
  input <- readFile "puzzle2.txt"
  let gifts = either (\_ -> [])
              id $
              runParser (some parseDimensions) "" input
      ribbons = map ribbonLength gifts
  print $ sum ribbons

ribbonLength dims =
      let vol = product dims
          smallestArea = perimeter . smallestSide $ dims
          in
          vol + smallestArea

parseDimensions :: Parsec String [Int]
parseDimensions = do
  l <- parseNum
  char 'x'
  w <- parseNum
  char 'x'
  h <- parseNum
  newline
  return [l,w,h]

parseNum = read <$> some digitChar

perimeter (x,y) = 2 * x + 2 * y
area (x,y) = x * y

smallestSide ps = minimumBy (\x y -> compare (area x) (area y)) $ allPairs ps

allPairs :: [a] -> [(a,a)]
allPairs ls = [(x,y) | (x:ys) <- tails ls, y <- ys]

p3_1 = do
  input <- readFile "puzzle3.txt"
  let houses = scanl followDirections (0,0) input
      uniqHouses = nub houses
    in print $ length uniqHouses

followDirections (x,y) '^' = (x, y + 1)
followDirections (x,y) '>' = (x + 1, y)
followDirections (x,y) 'v' = (x, y - 1)
followDirections (x,y) '<' = (x - 1, y)

p3_2 = do
  input <- readFile "puzzle3.txt"
  let aHouses = scanl followDirections (0,0) (evens input)
      bHouses = scanl followDirections (0,0) (odds input)
    in
    print $ length (nub $ aHouses ++ bHouses)


evens (_:x:xs) = x : evens xs
evens _ = []

odds (x:_:xs) = x : odds xs
odds (x:[]) = [x]
odds _ = []

p4_1 = do
  let allHashes = map (cathash "bgvyzdsv") [1..]
      misses = takeWhile (\h -> take 5 h /= "00000") allHashes
  print $ length misses

p4_2 = do
  let allHashes = map (cathash "bgvyzdsv") [1..]
      misses = takeWhile (\h -> take 6 h /= "000000") allHashes
  print $ length misses

cathash :: String -> Int -> String
cathash key = md5s . Str . (key ++) . show

p5 isNice = do
  input <- lines <$> readFile "puzzle5.txt"
  let faith = filter isNice input
  mapM_ (putStrLn . show) faith

p5_1 = p5 isNice1

isNice1 :: String -> Bool
isNice1 s = and [hasVowels s, hasDubs s, noVeto s]

hasVowels s = length (filter (\c -> elem c "aeiou") s) >= 3

hasDubs s = any dubs $ zip s (tail s)
  where dubs (x,y) = x == y

noVeto s = not . or $ map (flip isInfixOf s) ["ab", "cd", "pq", "xy"]

p5_2 = p5 isNice2

isNice2 s = and [dupes s, aba s]

dupes (a:b:cs) = (a:b:[]) `isInfixOf` cs || dupes (b:cs)
dupes _ = False

aba (a:b:c:ds)
  | a == c = True
  | True = aba (b:c:ds)
aba _ = False

-- Problem 6 --
data Instruction = Instruction { start :: Point, end :: Point, act :: SwitchAct }
                 deriving (Show)

data Point = Point (Int, Int)
           deriving (Show)

data SwitchAct = TurnOff | TurnOn | Toggle
               deriving (Show)

startingGrid = replicate 1000 $ replicate 1000 False

do6 TurnOff _ = False
do6 TurnOn _ = True
do6 Toggle x = not x

switchRow :: Int -> Int -> SwitchAct -> [Bool] -> [Bool]
switchRow start end instruction input =
  lMargin ++ changed ++ rMargin
  where dX = (end - start) + 1 -- +1 because coordinates are inclusive
        lMargin = take start input
        changed = map (do6 instruction) $ take dX $ drop start input
        rMargin = drop (end + 1) input

switchGrid :: Instruction -> [[Bool]] -> [[Bool]]
switchGrid (Instruction (Point (startX, startY)) (Point (endX, endY)) instruction) input =
  tMargin ++ changed ++ bMargin
  where dY = (endY - startY) + 1
        tMargin = take startY input
        changed = map (switchRow startX endX instruction) $ take dY $ drop startY input
        bMargin = drop (endY + 1) input

slurpInstructions = do
  rights . map (runParser parseInstruction "") . lines <$> readFile "puzzle6.txt"

newGrid n = replicate n $ replicate n False

p6_1 = do
  is <- slurpInstructions
  let final = foldl (flip switchGrid) (newGrid 1000) is
      count = sum $ map (\r -> length (filter id r)) final
  print count

testGrid :: Int -> [Instruction] -> IO ()
testGrid s is = showGrid $ foldl (flip switchGrid) (newGrid s) is

showGrid :: [[Bool]] -> IO ()
showGrid g = mapM_ putStrLn $ map showRow g

showRow :: [Bool] -> String
showRow = map (\c -> if c then 'O' else '.')

parseInstruction :: Parsec String Instruction
parseInstruction = do
  act <- parseAct
  start <- parsePoint
  space >> string "through" >> space
  end <- parsePoint
  return $ Instruction start end act

parseAct = do
  act <- choice [try parseToggle, try parseTurnOn, try parseTurnOff]
  space
  return act

parseTurnOn :: Parsec String SwitchAct
parseTurnOn = string "turn on" >> return TurnOn

parseTurnOff :: Parsec String SwitchAct
parseTurnOff = string "turn off" >> return TurnOff

parseToggle :: Parsec String SwitchAct
parseToggle = string "toggle" >> return Toggle

parsePoint :: Parsec String Point
parsePoint = do
  x <- parseInt
  char ','
  y <- parseInt
  return $ Point (x, y)

parseInt :: Parsec String Int
parseInt = do
  i <- read <$> some digitChar
  space
  return i
