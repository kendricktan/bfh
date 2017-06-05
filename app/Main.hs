{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.State.Lazy

initialCellState = replicate 30 (0 :: Int)

type CellState = [Int]

-- MoveState :: Int -> Parsed Commands -> CellState
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
parse :: [Command] -> State MoveState [Command]
parse [] = state (\x -> ([], x))
parse (x:xs) = if x == CloseBracket then s else s >>= parse
    where s = state(\v@(i, pc, cs) ->
                if i < 0 then error $ show v
                else
                case x of
                    MoveRight    -> (xs, (i + 1, pc ++ [x], cs))
                    MoveLeft     -> (xs, (i - 1, pc ++ [x], cs))
                    IncCell      -> (xs, (i, pc ++ [x], incCell cs i))
                    DecCell      -> (xs, (i, pc ++ [x], decCell cs i))
                    OpenBracket  -> let (xs', (i', pc', cs')) = runState (parse xs) (i, [], cs)
                                     in (xs', (i, pc ++ [x] ++ pc', cs'))
                    CloseBracket -> if cs !! i > 0
                                    then runState (parse (pc ++ [x] ++ xs)) (i, [], cs)
                                    else (xs, (i, pc ++ [x], cs))
                    _ -> (xs, v)
            )

main :: IO ()
main = do
    let (_, (_, _, cs)) = runState (parse $ tokenize "+++[>+++[>+<-]<-]") (0, [], initialCellState)
    print cs
