module AdventUtil where

import Data.Either
import Text.Megaparsec

slurpLinesWith parser fileName =
  rights . map (runParser parser "") . lines <$> readFile ("data/" ++ fileName)

slurp filename = readFile $ "data/" ++ filename

parseInt :: Parsec String Int
parseInt = read <$> some digitChar
