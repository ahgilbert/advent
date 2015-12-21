module P7 where

import Text.Megaparsec
import Control.Monad
import Data.Array.IO
import Data.Bits
import Data.Either
import Data.Maybe
import Data.Word
import AdventUtil

-- Problem 7 --
type Bus = Word16
type WireId = String
data Circuit = C { lhs :: LHS, rhs :: WireId }
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
data Circuit' = CMD { arrI :: Int, wid :: WireId, circuit :: Circuit }
              deriving (Show)
type P7Array = IOArray Int (Maybe Bus)

data ST = ST { cs :: [Circuit'], arr :: P7Array }


p7 = do
  input <- slurpLinesWith parseCircuitDeclaration "7_2.txt"
  let wires = zipWith (\c i -> CMD i (rhs c) c) input [1..] -- zip [1..] $ sort $ map rhs input
  arr <- newArray (1, length wires) Nothing
  answer <- exec arr wires "a"
  print answer

getWire :: [Circuit'] -> WireId -> LHS
getWire cs name = lhs $ circuit $ head $ filter (\c -> name == wid c) cs

exec :: P7Array -> [Circuit'] -> WireId -> IO Bus
exec arr cs w = do
  let c = head $ filter (\cmd -> wid cmd == w) cs
  cached <- readArray arr (arrI c)
  val <- if (isJust cached)
         then return $ fromJust cached
         else executeC arr cs ((lhs . circuit) c)
  when (isNothing cached) $ writeArray arr (arrI c) (Just val)
  return val

executeC :: P7Array -> [Circuit'] -> LHS -> IO Bus
executeC arr cs (Const (Fixed v)) = return v
executeC arr cs (Const (Wire w)) = executeC arr cs (getWire cs w)
executeC arr cs (Unary (Fixed v) Not) = return $ complement v
executeC arr cs (Unary (Wire w) Not) = complement <$> (exec arr cs w)
executeC arr cs (Binary (Fixed v1) (Fixed v2) op) = return $ (getOp op) v1 v2
executeC arr cs (Binary (Wire w) (Fixed v) op) = flip (getOp op) v <$> (exec arr cs w)
executeC arr cs (Binary (Fixed v) (Wire w) op) = (getOp op) v <$> (exec arr cs w)
executeC arr cs (Binary (Wire w1) (Wire w2) op) = do
  a <- exec arr cs w1
  b <- exec arr cs w2
  return $ (getOp op) a b

getOp :: BinaryGate -> Bus -> Bus -> Bus
getOp And a b = a .&. b
getOp Or a b= a .|. b
getOp LShift a b = shiftL a (fromIntegral b)
getOp RShift a b = shiftR a (fromIntegral b)

parseCircuitDeclaration :: Parsec String Circuit
parseCircuitDeclaration = do
  lhs <- parseLHS
  rhs <- some lowerChar
  return $ C lhs rhs

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

