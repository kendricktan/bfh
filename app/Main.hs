{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.State.Lazy

initialCellState = replicate 30 0

type CellState = [Int]

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
setCell f cs i = take (i - 1) cs ++ [f $ cs !! (i - 1)] ++ drop i cs

-- Parse
-- (looping stack) -> stack to be executed -> ...
parse :: [Command] -> [Command] -> CellState -> Int -> CellState
parse _ [] cs _ = cs
parse ls (x:xs) cs i
  | x == MoveRight = parse (ls ++ [x]) xs cs (i + 1)
  | x == MoveLeft = parse (ls ++ [x]) xs cs (i - 1)
  | x == IncCell = parse (ls ++ [x]) xs (setCell (+1) cs i) i
  | x == DecCell = parse (ls ++ [x]) xs (setCell (\j -> j - 1) cs i) i
  | x == OpenBracket = parse [] xs cs i
  | x == CloseBracket = if (cs !! (i - 1)) > 0
                           then parse [] (ls ++ [x] ++ xs) cs i
                           else parse [] xs cs i
  | otherwise = parse ls xs cs i


main :: IO ()
main = print $ parse [] (tokenize "+++++ [> +++++ < -]") initialCellState 1
