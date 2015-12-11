module Main where

import Text.Megaparsec
import Data.Either
import Data.List
import Data.Array.IO

main :: IO ()
main = p2_1

-- Problem 1 --
p1_1 = do
  instructions <- readFile "puzzle1.txt"
  let up = length $ filter (== '(') instructions
      down = length $ filter (== ')') instructions
   in putStrLn . show $ up - down

p1_2 = do
  instructions <- readFile "puzzle1.txt"
  print $ length instructions
  let faith = sumBraces instructions
      hope = length $ takeWhile (0 <=) faith
  print hope

sumBraces :: String -> [Int]
sumBraces instructions = scanl (\s c -> if c == '(' then s + 1 else s - 1) 0 instructions








-- Problem 2 --
p2_1 = do
  source <- readFile "puzzle2.txt"
  let gifts = runParser (some giftParser) "" source
      faith = either (\_ -> []) (map allPairs) gifts
      hope = map (sum . wrap . sort . (map prod)) faith
  print $ sum hope

giftParser :: Parsec String [Int]
giftParser = do
  l <- parseInt
  char 'x'
  w <- parseInt
  char 'x'
  h <- parseInt
  newline
  return [l,w,h]

parseInt = read <$> some digitChar

allPairs :: [a] -> [(a,a)]
allPairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

prod :: Num a => (a,a) -> a
prod (x,y) = x * y

wrap xs@(x:_) = x : map (2*) xs

perimeter (x,y) = (2 * x) + (2 * y)







-- Problem 3 --

p3_1 = do
  input <- readFile "puzzle3.txt"
  let houses = scanl followDirections (0,0) input
      uniqHouses = nub houses
    in print $ length uniqHouses

followDirections (x,y) '^' = (x,y + 1)
followDirections (x,y) '>' = (x + 1,y)
followDirections (x,y) 'v' = (x,y - 1)
followDirections (x,y) '<' = (x - 1,y)

p3_2 = do
  input <- readFile "puzzle3.txt"
  let aHouses = scanl followDirections (0,0) (evens input)
      bHouses = scanl followDirections (0,0) (odds input)
    in print $ length (nub $ aHouses ++ bHouses)

evens (_:x:xs) = x : evens xs
evens _ = []

odds (x:_:xs) = x : odds xs
odds (x:[]) = [x]
odds _ = []









-- Problem 4 --

p4_1 = do
  let allHashes = map (cathash "bgvyzdsv") [1..]
      misses = takeWhile (\h -> take 5 h /= "00000") allHashes
  print $ length misses

p4_2 = print "similar"

cathash key = undefined -- md5s . Str . (key ++) . show




















-- Problem 5 --
p5 isNice = do
  input <- lines <$> readFile "puzzle5.txt"
  let faith = filter isNice input
  print $ length faith
  --  mapM_ (putStrLn . show) faith

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
data Instruction = Instruction { start :: (Int,Int), end :: (Int,Int), act :: SwitchAct }
                 deriving (Show)

data SwitchAct = TurnOff | TurnOn | Toggle
               deriving (Show)

p6_1 = do
  is <- slurpInstructions
  let finalGrid = foldl (flip switchGrid) (newGrid 1000) is
      totalBrightness = sum $ map sum finalGrid
  print totalBrightness


newGrid n = replicate n $ replicate n 0

switchGrid (Instruction (startX, startY) (endX, endY) instruction) input =
  tMargin ++ changed ++ bMargin
  where dY = (endY - startY) + 1
        tMargin = take startY input
        changed = map (switchRow startX endX instruction) $ take dY $ drop startY input
        bMargin = drop (endY + 1) input

switchRow start end instruction input =
  lMargin ++ changed ++ rMargin
  where dX = (end - start) + 1 -- +1 because coordinates are inclusive
        lMargin = take start input
        changed = map (do6 instruction) $ take dX $ drop start input
        rMargin = drop (end + 1) input

do6 TurnOff x = if (x > 0) then x - 1 else 0
do6 TurnOn x = x + 1
do6 Toggle x = x + 2

testGrid :: Int -> [Instruction] -> IO ()
testGrid s is = showGrid $ foldl (flip switchGrid) (newGrid s) is

showGrid g = mapM_ putStrLn $ map show g



slurpInstructions = do
  rights . map (runParser parseInstruction "") . lines <$> readFile "puzzle6.txt"

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

parseTurnOn = string "turn on" >> return TurnOn
parseTurnOff = string "turn off" >> return TurnOff
parseToggle = string "toggle" >> return Toggle

parsePoint :: Parsec String (Int,Int)
parsePoint = do
  x <- parseInt'
  char ','
  y <- parseInt'
  return $ (x, y)

parseInt' :: Parsec String Int
parseInt' = do
  i <- read <$> some digitChar
  space
  return i

-- Problem 7 --

-- Problem 8 --
p8_1 = do
  input <- lines <$> readFile "puzzle8.txt"
  let mem = sum $ map sum $ rights $ map (runParser memSize "") input
      code = sum $ map codeSize input
  print code
  print mem
  print $ code - mem

codeSize = length

memSize = do
  char '\"'
  count <- option [0] $ some $ choice [try escapedChar, noneOf "\"" >> return 1]
  char '\"'
  return count

escapedChar :: Parsec String Int
escapedChar = do
  (char '\\')
  (choice [try (char '\\'),
           try (char '\"'),
           try (char 'x' >> hexDigitChar >> hexDigitChar)])
  return 1



p8_2 = do
  input <- lines <$> readFile "puzzle8.txt"
  let raw = map length input
      encoded = map (length . show) input
  print $ sum $ zipWith (-) encoded raw

-- Problem 9 --
data Distance = Distance { endpoints :: (String, String), dist :: Int }
              deriving (Show)
data Trip = Trip { route :: [String], distance :: Int }

p9_1 = do
  input <- rights <$> map (runParser parseDistance "") <$> lines <$> readFile "puzzle9.txt"
  mapM_ setDistances input
  let destinations = getLocations input
      trips = map measure $ permutations destinations
      tripLengths = minimumBy (\a b -> compare (distance a) (distance b)) trips
  print "later"

parseDistance :: Parsec String Distance
parseDistance = do
  pointA <- some letterChar
  string " to "
  pointB <- some letterChar
  string " = "
  distance <- read <$> some digitChar
  return $ Distance (pointA, pointB) distance

getLocations :: [Distance] -> [String]
getLocations ds = nub $ concatMap ((\(a,b) -> [a,b]) . endpoints) ds

setDistances :: Distance -> IO ()
setDistances = undefined

measure :: [String] -> Trip
measure = undefined











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
