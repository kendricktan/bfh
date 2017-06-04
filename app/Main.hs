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
setCell f cs i = take i cs ++ [if j < 0 then 255 else j] ++ drop (i + 1) cs
    where j = f $ cs !! i

-- Parse
-- Saving all the states to debug
parse :: [Command] -> [Command] -> CellState -> [Int] -> ([Command], [Command], CellState, [Int])
parse ls [] cs i = (ls, [], cs, i)
parse ls fx@(x:xs) cs i
  | last i < 0 = error (show (ls, fx, cs, i))
  | x == MoveRight = parse (ls ++ [x]) xs cs (i ++ [last i + 1])
  | x == MoveLeft = parse (ls ++ [x]) xs cs (i ++ [last i - 1])
  | x == IncCell = parse (ls ++ [x]) xs (setCell (+1) cs (last i)) (i ++ [last i])
  | x == DecCell = parse (ls ++ [x]) xs (setCell (\j -> j - 1) cs (last i)) (i ++ [last i])
  | x == OpenBracket = let (ls', xs', cs', i') = parse [] xs cs [last i]
                        in parse (ls ++ [x] ++ ls') xs' cs' (i ++ i')
  | x == CloseBracket = if (cs !! last i) > 0
                           then parse [] (ls ++ [x] ++ xs) cs (i ++ [last i])
                           else (ls ++ [x], xs, cs, i)
  | otherwise = parse (ls ++ [x]) xs cs i


main :: IO ()
main = do
    let (_, _, cs, _) = parse [] (tokenize "+++[>+++[>+<-]<-]") initialCellState [0]
    print cs
