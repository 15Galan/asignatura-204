-------------------------------------------------------------------------------
-- Dijkstra's two stacks algorithm to evaluate expressions
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Stack.Dijkstra2Stacks where

import Data.Char (isDigit, ord)
import DataStructures.Stack.LinearStack

toInt :: Char -> Int
toInt c = ord c - ord '0'

isOp :: Char -> Bool
isOp c = c `elem` "+-*"

type Op = Int -> Int -> Int

toOp :: Char -> Op
toOp '+' = (+)
toOp '-' = (-)
toOp '*' = (*)

topPop :: Stack a -> (a, Stack a)
topPop s = (top s, pop s)

eval :: String -> Int
eval xs = aux xs empty empty

aux :: String -> Stack Int -> Stack Op -> Int
aux []     sNum sOp = top sNum
aux (x:xs) sNum sOp
 | x `elem` " (" = aux xs sNum sOp
 | isOp x        = aux xs sNum (push (toOp x) sOp)
 | isDigit x     = aux xs (push (toInt x) sNum) sOp
 | x == ')'      = aux xs (push n3 sNum'') sOp'
 where
  (n1,sNum') = topPop sNum
  (n2,sNum'') = topPop sNum'
  (op,sOp') = topPop sOp
  n3 = op n2 n1

test = eval "((5*(1+2))+(9-3))"

