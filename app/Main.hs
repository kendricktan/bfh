{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.State.Lazy
import           Data.Char
import           System.Environment

initialCellState = replicate 30000 (0 :: Int)

type CellState = [Int]

-- MoveState :: Int (Position) -> Parsed Commands -> CellState
type MoveState = (Int, [Command], CellState)

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
tokenize [] = []
tokenize (x:xs)
  | x == '>' = MoveRight : tokenize xs
  | x == '<' = MoveLeft : tokenize xs
  | x == '+' = IncCell: tokenize xs
  | x == '-' = DecCell : tokenize xs
  | x == '.' = PrintCell : tokenize xs
  | x == ',' = InputCell: tokenize xs
  | x == '[' = OpenBracket : tokenize xs
  | x == ']' = CloseBracket: tokenize xs
  | otherwise = tokenize xs


-- Sets a specific cell to a value
setCell :: (Int -> Int) -> CellState -> Int -> CellState
setCell f cs i = take i cs ++ [if j < 0 then 255 else j] ++ drop (i + 1) cs
    where j = f $ cs !! i

incCell :: CellState -> Int -> CellState
incCell = setCell (+1)

decCell :: CellState -> Int -> CellState
decCell = setCell (\x -> x - 1)


-- Parse
-- Close bracket is different from the others as it
-- doesn't bind itself to the parse process
parse :: [Command] -> StateT MoveState IO [Command]
parse [] = StateT (\x -> pure ([], x))
parse (x:xs) = if x == CloseBracket then s else s >>= parse
    where s = StateT (\v@(i, pc, cs) ->
                if i < 0 then error $ show v
                else
                case x of
                    MoveRight    -> pure (xs, (i + 1, pc ++ [x], cs))
                    MoveLeft     -> pure (xs, (i - 1, pc ++ [x], cs))
                    IncCell      -> pure (xs, (i, pc ++ [x], incCell cs i))
                    DecCell      -> pure (xs, (i, pc ++ [x], decCell cs i))
                    OpenBracket  -> do (xs', (i', pc', cs')) <- liftIO $ runStateT (parse xs) (i, [], cs)
                                       pure (xs', (i, pc ++ [x] ++ pc', cs'))
                    CloseBracket -> if cs !! i > 0
                                    then
                                       do (xs', ms') <- liftIO $ runStateT (parse (pc ++ [x] ++ xs)) (i, [], cs)
                                          pure (xs', ms')
                                    else pure (xs, (i, pc ++ [x], cs))
                    InputCell    -> do c <- liftIO getChar
                                       pure (xs, (i, pc ++ [x], setCell (const $ fromEnum c) cs i))
                    PrintCell    -> do liftIO $ print (chr (cs !! i))
                                       return (xs, (i, pc ++ [x], cs))
            )

main :: IO ()
main = do
    (file : _) <- getArgs
    s <- readFile file
    (_, (_, _, cs)) <- runStateT (parse $ tokenize s) (0, [], initialCellState)
    return ()
