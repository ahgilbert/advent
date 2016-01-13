module P23 where

import AdventUtil
import Text.Megaparsec
import qualified Control.Monad.State as S
import Control.Monad.State.Puts

type TState = S.State Turing
data Turing = Turing { prog :: [TLock],
                       tick :: Int,
                       a :: Integer,
                       b :: Integer }

data TLock = Hlf { r :: (Turing -> Integer) }
           | Tpl { r :: (Turing -> Integer) }
           | Inc { r :: (Turing -> Integer) }
           | Jmp { off :: Int }
           | JIE { r :: (Turing -> Integer), off :: Int }
           | JIO { r :: (Turing -> Integer), off :: Int }

instance Show TLock where
  show (Hlf _) = "hlf"
  show (Tpl _) = "tpl"
  show (Inc _) = "inc"
  show (Jmp n) = "jmp " ++ show n
  show (JIE _ n) = "jie " ++ show n
  show (JIO _ n) = "jio " ++ show n

p23 = do
  instructions <- slurpLinesWith parseTLock "23.txt"
  return instructions

exec23 :: TLock -> TState ()
exec23 (Hlf r) = do
  v <- S.gets r
  puts r $ div v 2
exec23 (Tpl r) = do
  v <- S.gets r
  puts r $ v * 3
exec23 _ = return ()


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
  off <- (optional $ char '+') >> parseInt
  return (Jmp off)

parseJIE = do
  string "jie "
  reg <- parseRegister
  space
  off <- (optional $ char '+') >> parseInt
  return (JIE reg off)

parseJIO = do
  string "jio "
  reg <- parseRegister
  space
  off <- (optional $ char '+') >> parseInt
  return (JIO reg off)

parseRegister = choice [char 'a' >> return a,
                        char 'b' >> return b]
