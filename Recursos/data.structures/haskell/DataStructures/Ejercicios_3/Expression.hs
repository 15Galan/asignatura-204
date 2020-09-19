module Expression (
    Item (..)    -- Importa "Item" y todos sus tipos ("Add", "Dif", "Mul"...)
    , Expression
    , value
    , showExpr
    , sample1,sample2
    ) where


data Item = Add | Dif | Mul | Value Integer | LeftP | RightP deriving Show

type Expression = [Item]


-- Ejemplos de expresiones (necesita siempre un paréntesis al princicpio y al final):

sample1 :: Expression
sample1 = [LeftP, Value 5, Add, LeftP, Value 6, Dif, Value 2, RightP, Mul, Value 3, RightP]
--      = (5 + (6 - 2) * 3)

sample2 :: Expression
sample2 = [LeftP, LeftP, Value 4, Mul, Value 5, RightP, Dif, Value 6, RightP]
--      = ((4 * 5) - 6)


---------------------------------------
-- Ejercicio 4
---------------------------------------

value :: (Num a) => Item -> a -> a -> a
value Add x y = x + y
value Dif x y = x - y
value Mul x y = x * y


showExpr :: Expression -> String
showExpr []             = ""                      -- Caso base
showExpr (Add:xs)       = " + " ++ showExpr xs
showExpr (Dif:xs)       = " - " ++ showExpr xs    -- Ya que "Expression = [Item]", es
showExpr (Mul:xs)       = " * " ++ showExpr xs    -- posible usar la nomenclatura para
showExpr (LeftP:xs)     = "(" ++ showExpr xs      -- listas "(x:xs)", siendo "x" una
showExpr (RightP:xs)    = ")" ++ showExpr xs      -- variable de tipo "Item".
showExpr ((Value x):xs) = show x ++ showExpr xs
