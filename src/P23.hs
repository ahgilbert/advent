module P23 where

import AdventUtil
import Text.Megaparsec
import qualified Control.Monad.State.Lazy as S

type TState = S.State Turing
data Turing = Turing { tick :: Int, a :: Integer, b :: Integer }

data TLock = Hlf { r :: (Turing -> Integer) }
           | Tpl { r :: (Turing -> Integer) }
           | Inc { r :: (Turing -> Integer) }
           | Jmp { off :: Int }
           | JIE { r :: (Turing -> Integer), off :: Int }
           | JIO { r :: (Turing -> Integer), off :: Int }

p23 = do
  instructions <- slurpLinesWith parseTLock "23.txt"
  return instructions

runTuring :: TState Int
runTuring = do
  undefined

runOneTuring :: [TLock] -> TState ()
runOneTuring = undefined



-- Parsers --
parseTLock :: Parsec String TLock
parseTLock = do
  choice [try parseHlf, try parseTpl, try parseInc, try parseJmp, try parseJIE, try parseJIO]

parseHlf :: Parsec String TLock
parseHlf = do
  string "hlf "
  reg <- parseRegister
  return (Hlf reg)

parseTpl = do
  string "tpl "
  reg <- parseRegister
  return (Tpl reg)

parseInc = do
  string "inc "
  reg <- parseRegister
  return (Inc reg)

parseJmp = do
  string "jmp "
  off <- parseInt
  return (Jmp off)

parseJIE = do
  string "jie "
  reg <- parseRegister
  space
  off <- parseInt
  return (JIE reg off)

parseJIO = do
  string "jio "
  reg <- parseRegister
  space
  off <- parseInt
  return (JIO reg off)

parseRegister = choice [char 'a' >> return a,
                        char 'b' >> return b]
