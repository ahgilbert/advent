module P12 where

import AdventUtil
import Text.JSON
import Text.JSON.String
import Text.Megaparsec
import Data.Maybe
import Data.Either

-- Problem 12 --
p12 = do
  input <- slurp "12.txt"
  let faith = runParser parseJson "" input
      hope = either (\_ -> 0) id (runParser parseJson "" input)
  print hope

parseJson :: Parsec String Int
parseJson = do
  faith <- some (choice [parseNum, noneOf "{[]}" >> return 0])
  return $ sum faith

parseNum :: Parsec String Int
parseNum = do
  sign <- optional $ char '-'
  num <- some digitChar
  let n = read num :: Int
  if (isJust sign)
    then return ((-1) * n)
    else return n

parseList = between (char '[') (char ']') parseJson

parseMap = between (char '{') (char '}') parseMapInnards

parseMapInnards = do
  choice [parseList, parseMap]

parseRed :: Parsec String String
parseRed = string ":\"red\""

goodMap = "{\"a\":12,\"b\":\"food\",\"c\":90}"
badMap = "{\"a\":12,\"b\":\"red\",\"c\":90}"


p12_2 = do
  inp <- slurp "12.txt"
  let json = head $ rights [runGetJSON readJSValue inp]
  print $ jsNums json

jsNums :: JSValue -> Int
jsNums JSNull = 0
jsNums (JSBool _) = 0
jsNums (JSString _) = 0
jsNums (JSArray js) = sum $ map jsNums js
jsNums (JSObject os) = let
  assoc = fromJSObject os
  trash = isRed assoc
  in if trash then 0 else sum $ map (jsNums . snd) (fromJSObject os)
jsNums (JSRational _ x) = truncate x

isRed :: [(String, JSValue)] -> Bool
isRed ps = 0 < (length $ filter isRed' (map snd ps))
  where
    isRed' (JSString v)
      | v == (toJSString "red") = True
      | True = False
    isRed' _ = False

