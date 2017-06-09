module Main where

import           Control.Monad.State.Lazy
import           Data.Char
import           Data.Maybe
import           System.Environment

type CellState = [Int]

data MoveState
  = MoveState
  { position :: Int
  , commands :: [Command]
  , cells :: CellState
  } deriving Show

initialMoveState :: MoveState
initialMoveState
  = MoveState
  { position = 0
  , commands = []
  , cells = replicate 30000 0
  }

data Command = MoveRight
             | MoveLeft
             | IncCell
             | DecCell
             | PrintCell
             | InputCell
             | OpenBracket
             | CloseBracket
             deriving (Show, Eq)

tokenize :: String -> [Command]
tokenize = catMaybes . fmap charToCommand
  where
    charToCommand c =
      case c of
        '>' -> Just MoveRight
        '<' -> Just MoveLeft
        '+' -> Just IncCell
        '-' -> Just DecCell
        '.' -> Just PrintCell
        ',' -> Just InputCell
        '[' -> Just OpenBracket
        ']' -> Just CloseBracket
        _ -> Nothing

setCell :: (Int -> Int) -> CellState -> Int -> CellState
setCell _ [] _ = []
setCell f (c:rest) 0 = f c : rest
setCell f (c:rest) n = c : setCell f rest (n-1)

incCell :: CellState -> Int -> CellState
incCell = setCell (+1)

decCell :: CellState -> Int -> CellState
decCell = setCell (subtract 1)

-- Parse
-- Close bracket is different from the others as it
-- doesn't bind itself to the parse process
parse :: [Command] -> StateT MoveState IO [Command]
parse [] = pure [] 
parse (x:xs) =
  case x of
    CloseBracket -> s
    _ -> s >>= parse
  where
    s = do
      ms@(MoveState pos cmds cls) <- get 
      when (pos < 0) . error $ show ms
      case x of
        MoveRight -> do
          put $ MoveState (pos + 1) (cmds ++ [x]) cls
          pure xs
        MoveLeft -> do
          put $ MoveState (pos - 1) (cmds ++ [x]) cls
          pure xs
        IncCell -> do
          put $ MoveState pos (cmds ++ [x]) (incCell cls pos)
          pure xs
        DecCell -> do
          put $ MoveState pos (cmds ++ [x]) (decCell cls pos)
          pure xs 
        OpenBracket -> do
          modify $ \ms -> ms { commands = [] }
          xs' <- parse xs
          MoveState _ cmds' cls' <- get
          put $ MoveState pos (cmds ++ [x] ++ cmds') cls'
          pure xs'
        CloseBracket ->
          if cls !! pos > 0
            then do
              modify $ \ms -> ms { commands = [] }
              parse (cmds ++ x:xs)
            else do
              put $ MoveState pos (cmds ++ [x]) cls
              pure xs
        InputCell -> do
          c <- liftIO getChar
          put $ MoveState pos (cmds ++ [x]) (setCell (const $ fromEnum c) cls pos)
          pure xs
        PrintCell -> do
          liftIO $ print (chr (cls !! pos))
          put $ MoveState pos (cmds ++ [x]) cls
          pure xs

main :: IO ()
main = do
    (file : _) <- getArgs
    s <- readFile file
    void $ runStateT (parse $ tokenize s) initialMoveState
