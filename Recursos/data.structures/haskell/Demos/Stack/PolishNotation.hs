-------------------------------------------------------------------------------
-- Polish Notation evaluator 
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Stack.PolishNotation where

import Data.Char (isDigit, ord, chr)
import DataStructures.Stack.LinearStack

type Digit = Int

-- toDigit '7' => 7
toDigit :: Char -> Digit
toDigit c 
 | '0' <= c && c <= '9' = ord c - ord '0'
 | otherwise            = error (c : " is not a digit")

-- toChar 7 => '7'
toChar :: Digit -> Char
toChar n
 | 0 <= n && n <= 9 = chr (ord '0' + n)
 | otherwise        = error (show n++" is not a single digit")

-- is this char an operator?
isOp :: Char -> Bool
isOp c = c `elem` "+-*/"

type Op = Digit -> Digit -> Digit

toOp :: Char -> Op
toOp '+' = (+)
toOp '-' = (-)
toOp '*' = (*)
toOp '/' = div

topPop :: Stack a -> (a, Stack a)
topPop s = (top s, pop s)

eval :: String -> Int
eval xs = aux xs empty 

aux :: String -> Stack Char -> Int
aux xs s
 | twoDigitsOnTop s = aux xs (push (toChar v) s3)
 where
  (y,s1)  = topPop s
  (x,s2)  = topPop s1
  (z,s3) = topPop s2
  v = (toOp z) (toDigit x) (toDigit y) 
aux []     s        = toDigit (top s)
aux (x:xs) s
 | x `elem` " "     = aux xs s
 | isOp x           = aux xs (push x s) 
 | isDigit x        = aux xs (push x s)  
 | otherwise        = error ("token "++[x]++" unexpected")


-- returns True if two top elements in stacks are numbers (isDigit)
twoDigitsOnTop :: Stack Char -> Bool
twoDigitsOnTop s = not (isEmpty s) && isDigit (top s) &&
                   not (isEmpty s') && isDigit (top s') 
 where 
  s' = pop s


test = eval "- 7 + 1 2 " 

test2 = eval "+ * / 5 - 7 + 1 1 3 + 2 + 1 1"

