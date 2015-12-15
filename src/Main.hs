module Main where

import Text.Megaparsec
import Data.Bits
import Data.Char
import Data.Either
import Data.List
import Data.Word
import Data.Array.IO
import Data.Maybe

main :: IO ()
main = p2_1

slurpLinesWith parser fileName =
  rights . map (runParser parser "") . lines <$> readFile ("data/" ++ fileName)

slurp filename = readFile $ "data/" ++ filename

-- Problem 1 --
p1_1 = do
  instructions <- readFile "data/puzzle1.txt"
  let up = length $ filter (== '(') instructions
      down = length $ filter (== ')') instructions
   in putStrLn . show $ up - down

p1_2 = do
  instructions <- readFile "data/puzzle1.txt"
  print $ length instructions
  let faith = sumBraces instructions
      hope = length $ takeWhile (0 <=) faith
  print hope

sumBraces :: String -> [Int]
sumBraces instructions = scanl (\s c -> if c == '(' then s + 1 else s - 1) 0 instructions








-- Problem 2 --
p2_1 = do
  source <- readFile "data/puzzle2.txt"
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
  input <- readFile "data/puzzle3.txt"
  let houses = scanl followDirections (0,0) input
      uniqHouses = nub houses
    in print $ length uniqHouses

followDirections (x,y) '^' = (x,y + 1)
followDirections (x,y) '>' = (x + 1,y)
followDirections (x,y) 'v' = (x,y - 1)
followDirections (x,y) '<' = (x - 1,y)

p3_2 = do
  input <- readFile "data/puzzle3.txt"
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
  input <- lines <$> readFile "data/puzzle5.txt"
  let faith = filter isNice input
  print $ length faith
  --  mapM_ (putStrLn . show) faith

p5_1 = p5 isNice1

isNice1 :: String -> Bool
isNice1 s = and [hasVowels s, hasDubs s, noVeto ["ab", "cd", "pq", "xy"] s]

hasVowels s = length (filter (\c -> elem c "aeiou") s) >= 3

hasDubs s = any dubs $ zip s (tail s)
  where dubs (x,y) = x == y

noVeto blackList s = not . or $ map (flip isInfixOf s) blackList

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
  rights . map (runParser parseInstruction "") . lines <$> readFile "data/puzzle6.txt"

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
type Bus = Word16
type WireId = String

data Circuit = Circuit { lhs :: LHS, rhs :: WireId }
                        deriving Show

data LHS = Const { v :: Input }
         | Unary { i :: Input, uOp :: UnaryGate }
         | Binary { a :: Input, b :: Input, bOp :: BinaryGate }
         deriving Show

data UnaryGate = Not
               deriving Show

data BinaryGate = And | Or | LShift | RShift
                deriving Show

data Input = Fixed { strength :: Bus }
           | Wire { wire :: WireId }
           deriving Show

p7 = do
  input <- slurpLinesWith parseCircuitDeclaration "puzzle7.txt"
  let wires = zip [1..] $ sort $ map rhs input
  print $ length wires

parseCircuitDeclaration :: Parsec String Circuit
parseCircuitDeclaration = do
  lhs <- parseLHS
  rhs <- some lowerChar
  return $ Circuit lhs rhs

parseInput = choice [parseConst, parseWireId]

parseConst :: Parsec String Input
parseConst = do
  v <- read <$> some digitChar
  return (Fixed v)

parseWireId :: Parsec String Input
parseWireId = do
  name <- some lowerChar
  return (Wire name)

parseUnaryGate :: Parsec String UnaryGate
parseUnaryGate = string "NOT" >> return Not

parseBinaryGate :: Parsec String BinaryGate
parseBinaryGate = some upperChar >>= getOp
  where getOp "AND" = return And
        getOp "OR" = return Or
        getOp "LSHIFT" = return LShift
        getOp "RSHIFT" = return RShift
        getOp w = fail ("unrecognized binary operation: " ++ w)

parseLHS :: Parsec String LHS
parseLHS = do
  ret <- choice [try parseBinaryExp, try parseUnaryExp, try parseConstExp, try parseSolder]
  string " -> "
  return ret

parseSolder :: Parsec String LHS
parseSolder = do
  v <- some lowerChar
  return (Const (Wire v))

parseConstExp :: Parsec String LHS
parseConstExp = do
  v <- read <$> some digitChar
  return (Const (Fixed v))

parseUnaryExp = do
  op <- parseUnaryGate
  space
  i <- parseInput
  return $ Unary i op

parseBinaryExp = do
  a <- parseInput
  space
  op <- parseBinaryGate
  space
  b <- parseInput
  return $ Binary a b op








































  -- Problem 8 --
p8_1 = do
  input <- lines <$> readFile "data/puzzle8.txt"
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
  input <- lines <$> readFile "data/puzzle8.txt"
  let raw = map length input
      encoded = map (length . show) input
  print $ sum $ zipWith (-) encoded raw








-- Problem 9 (crying for better state management...) --

data Distance = Distance { pointA :: String, pointB :: String, dist :: Int }
              deriving Show
data Location = Location { name :: String, idx :: Int }
              deriving Show
data Trip = Trip { route :: [String], total :: Int }
          deriving Show

instance Ord Trip where
  compare a b = compare (total a) (total b)
instance Eq Trip where
  a == b = total a == total b && route a == route b

p9 = do
  (keys, arr) <- slurp9
  distances <- mapM (getTripLength arr) (permutations keys)
  let shortest = minimum distances
      longest = maximum distances
  putStrLn "shortest:"
  print shortest
  putStrLn ""
  putStrLn "longest:"
  print longest

slurp9 :: IO ([Location], IOArray Int Int)
slurp9 = do
  distances <- slurpLinesWith parseDistance "puzzle9.txt"
  let locNames = sort . nub . concatMap (\d -> [pointA d, pointB d]) $ distances
      locs = zipWith (\n i -> Location n i) locNames [0..]
      numPairs = (length locs) ^ 2 - 1
  arr <- newArray (0, numPairs) 0
  mapM_ (setDistances arr locs) distances
  return (locs, arr)

setDistances :: IOArray Int Int -> [Location] -> Distance -> IO ()
setDistances arr keyset (Distance a b d) = do
  let n = length keyset
      idxer = cityNum keyset
      flattr = coordFlatten n
  writeArray arr (flattr (idxer a) (idxer b)) d
  writeArray arr (flattr (idxer b) (idxer a)) d

coordFlatten :: Int -> Int -> Int -> Int
coordFlatten n r c = (n * r) + c

cityNum :: [Location] -> String -> Int
cityNum locs target = idx . head $ filter (\(Location n i) -> n == target) locs

parseDistance :: Parsec String Distance
parseDistance = do
  pointA <- some letterChar
  string " to "
  pointB <- some letterChar
  string " = "
  distance <- read <$> some digitChar
  return $ Distance pointA pointB distance

getTripLength :: IOArray Int Int -> [Location] -> IO Trip
getTripLength arr locs = do
  let hops = zip locs (tail locs)
      n = length locs
      idxer = coordFlatten n
  dists <- mapM (getDistance arr n) hops
  return (Trip (map name locs) (sum dists))

getDistance :: IOArray Int Int -> Int -> (Location, Location) -> IO Int
getDistance arr n (a,b) = readArray arr (coordFlatten n (idx a) (idx b))




















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















-- Problem 12 --
p12 = do
  input <- slurp "puzzle12.txt"
  let faith = runParser parseJson "" input
      hope = either (\_ -> 0) id (runParser parseJson "" input)
  print hope

parseJson :: Parsec String Int
parseJson = do
  faith <- some (choice [parseNum, asciiChar >> return 0])
  return $ sum faith

parseNum :: Parsec String Int
parseNum = do
  sign <- optional $ char '-'
  num <- some digitChar
  let n = read num :: Int
  if (isJust sign)
    then return ((-1) * n)
    else return n

-- Problem 13, aka "Problem 9 eats its tail" --

{- same as traveling salesman, but:
     A Location is a Guest
     A Trip is a SeatingChart
     I must consider returning home
-}





















-- Problem 14 - Reindeer Olympics --
data Reindeer = R { moniker :: String,
                    moving :: (Int, Int),
                    rest :: Int }
              deriving (Show)

timer = 2503

p14_1 = do
  input <- slurpLinesWith parseReindeer "puzzle14.txt"
  let race = map (\r -> (r, getReindeerLoc r timer)) input
      race' = sortBy (\a b -> compare (snd b) (snd a)) race
  mapM print race'

p14_2 = do
  deer <- slurpLinesWith parseReindeer "puzzle14.txt"
  let slices = map (getReindeerLocs deer) [1..timer]
      leaders = map scoreTimeslice slices
      points = map (foldl (+) 0) (transpose leaders)
  print $ sort points

scoreTimeslice :: [Int] -> [Int]
scoreTimeslice distances =
  let maxDistance = maximum distances
  in map (\d -> if d == maxDistance then 1 else 0) distances

parseReindeer :: Parsec String Reindeer
parseReindeer = do
  moniker <- some letterChar
  string " can fly "
  speed <- parseInt
  string " km/s for "
  stamina <- parseInt
  string " seconds, but then must rest for "
  rest <- parseInt
  return (R moniker (speed, stamina) rest)

getReindeerLocs :: [Reindeer] -> Int -> [Int]
getReindeerLocs rs clock = map (flip getReindeerLoc clock) rs

getReindeerLoc :: Reindeer -> Int -> Int
getReindeerLoc deer@(R n (speed, stamina) rest) clock
  | clock > (stamina + rest) =
    let burstDist = speed * stamina
        intervals = quot clock (stamina + rest)
        partial = rem clock (stamina + rest)
    in (burstDist * intervals) + (getReindeerLoc deer partial)
  | True = (speed * (min stamina clock))

isFlying :: Reindeer -> Int -> Bool
isFlying r@(R _ (_, stamina) rest) clock =
  if (clock <= stamina)
  then True
  else not $ isResting r (clock - stamina)

isResting :: Reindeer -> Int -> Bool
isResting r@(R _ (_, stamina) rest) clock =
  if (clock <= rest)
  then True
  else not $ isFlying r (clock - rest)
