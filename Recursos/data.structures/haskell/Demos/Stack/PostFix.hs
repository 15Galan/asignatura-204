module PostFix (
   evaluatePostFix
 ) where

import DataStructures.Stack.LinearStack
import Expression


evaluatePostFix :: Expression -> Integer
evaluatePostFix ex = eval ex empty

eval :: Expression -> Stack Integer -> Integer
eval [ ] 		s     = top s
eval (Value x : ts)	s     = eval ts (push x s)
eval (op:ts) 		v2v1s = eval ts (push (value op v1 v2) s) 
  where v2  = top v2v1s  -- segundo arg
        v1s = pop v2v1s  -- el stack sin segundo arg
        v1  = top v1s    -- primer arg
        s   = pop v1s    -- el stack sin los dos args