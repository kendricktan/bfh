{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.State.Lazy

initialCellState = replicate 10 0

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
setCell f cs i
  | i == 0    = f (head cs) : tail cs
  | otherwise = take (i - 1) cs ++ [f $ cs !! (i - 1)] ++ drop i cs

parse :: [Command] -> CellState -> Int -> CellState
parse [] cs _ = cs
parse (x:xs) cs i
  | x == MoveRight = parse xs cs (i + 1)
  | x == MoveLeft = parse xs cs (i - 1)
  | x == IncCell = parse xs (setCell (+1) cs i) i
  | x == DecCell = parse xs (setCell (\j -> j - 1) cs i) i
  | otherwise = cs

main :: IO ()
main = print $ parse (tokenize "+++++ >> +++++ << -- ") initialCellState 0
