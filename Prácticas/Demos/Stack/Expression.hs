module Expression (
   Item (..)       -- data Item = Add | Dif | . . .

 , Expression      -- type Expression = [Item]
 , value           -- :: Item -> Integer -> Integer -> Integer
 , show'           -- :: Expression -> String
 , sample1
 , sample2
   ) where


data Item   = Add | Dif | Mul | Value Integer    deriving Show
type Expression = [Item]

-- sample1 ---> 5 + 6 - 2 * 4
sample1 = 	[Value 5, Value 6, Add,
		 Value 2, Value 4, Mul, Dif]

--la expresión anterior en Polaca Inversa
sample2 = [Value 5, Value 6, Value 2, Dif, Value 3, Mul, Add]

value :: Item -> Integer -> Integer -> Integer
value Add x y = x + y
value Dif x y = x - y
value Mul x y = x * y

show' :: Expression -> String
show' []              = ""
show'  (Value x : ts) = ' ' : show x ++ show' ts
show'  (Add : ts)    = '+' : show' ts
show'  (Dif : ts)    = '-' : show' ts
show'  (Mul : ts)    = '*' : show' ts
