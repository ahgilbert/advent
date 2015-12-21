module P6 where

import Data.Either
import Text.Megaparsec

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
  rights . map (runParser parseInstruction "") . lines <$> readFile "data/6.txt"

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

