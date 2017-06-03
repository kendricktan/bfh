module Main where

import Data.Char

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           | TokSpace
           deriving (Show, Eq)

data Operator = Plus | Minus | Times | Divide deriving (Show, Eq)

operator :: Char -> Operator
operator c
  | c == '+' = Plus
  | c == '-' = Minus
  | c == '*' = Times
  | c == '/' = Divide

opToStr :: Operator -> String
opToStr Plus   = "+"
opToStr Minus  = "-"
opToStr Times  = "*"
opToStr Divide = "/"

showContent :: Token -> String
showContent (TokOp op)     = opToStr op
showContent (TokIdent str) = str
showContent (TokNum i)     = show i

tokenizeChar :: Char -> Token
tokenizeChar c
  | c `elem` "+-*/" = TokOp (operator c)
  | isDigit c = TokNum (digitToInt c)
  | isAlpha c = TokIdent [c]
  | isSpace c = TokSpace
  | otherwise = error $ "Cannot tokenize " ++ [c]

tokenize :: String -> [Token]
tokenize = map tokenizeChar

main :: IO ()
main = print $ operator '*'
