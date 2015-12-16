import Control.Monad.State
import System.Random

main :: IO ()
main = runStateT code [1..] >> return ()

code :: StateT [Integer] IO ()
code = do
  x <- pop
  io $ print x
  y <- pop
  io $ print y
  return ()

pop :: StateT [Integer] IO Integer
pop = do
  (x:xs) <- get
  put xs
  return x

io :: IO a -> StateT [Integer] IO a
io = liftIO

game = do answer <- getStdRandom (randomR (1,100))
          putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
          guesses <- execStateT (guessSession answer) 0
          putStrLn $ "Success in " ++ (show guesses) ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer =
  do g <- read <$> lift getLine
     modify (+1)
     case compare g answer of
       LT -> do lift $ putStrLn "too low"
                guessSession answer
       GT -> do lift $ putStrLn "Too high"
                guessSession answer
       EQ -> lift $ putStrLn "Got it!"

data ST = ST { i :: Int, xs :: [()] }
        deriving (Show)

faith = do
  (_,a) <- runStateT hope (ST 0 [])
  (_,b) <- runStateT hope a
  c <- runStateT hope b
  return c

hope :: StateT ST IO Int
hope = do
  x <- gets xs
  liftIO $ print x
  put (ST  1 (():x))
  y <- gets xs
  liftIO $ print y
  liftIO getLine
  return $ length y
