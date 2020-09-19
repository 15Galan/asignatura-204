import DataStructures.Trees

data Expr = Value Integer | Add Expr Expr | Diff Expr Expr | Mult Expr Expr deriving Show

e1 :: Expr
e1 = Mult (Add (Value 1) (Value 2)) (Value 3)


evaluate :: Expr -> Integer
evaluate Empty = 0
evaluate e | 

toRPN :: Expr -> String
toRPN e = 
    where
        aux (Value x) = " " ++ x ++ " "
        aux (Add)     = " + "
        aux (Diff)    = " - "
        aux (Mult)    = " * "



foldExpr :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr ifValue ifAdd ifDiff ifMult e = fun e
    where
        fun (Value x)    = ifValue x
        fun (Add e1 e2)  = ifAdd (fun e1) (fun e2)
        fun (Diff e1 e2) = ifDiff (fun e1) (fun e2)
        fun (Mult e1 e2) = ifMult (fun e1) (fun e2)


evaluate' :: Expr -> Integer
evaluate' Empty = 0
evaluate' e =  foldExpr undefined
