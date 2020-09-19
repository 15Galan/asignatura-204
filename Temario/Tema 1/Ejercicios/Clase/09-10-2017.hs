import Test.QuickCheck

square x = x*x

p1 x y =		True		==> square x + square y + 2*x*y

p2 x y =		True		==> abs(x + y) == abs + abs y

p3 x y =	x > 0 && y > 0	==> abs(x + y) == abs + abs y



fib :: Integer -> Integer		{- Patrones -}
fib 0 = 1
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

fib':: Integer -> Integer		{- Guardas -}
fib' x	| x == 0	= 1
		| x == 1	= 1
		| otherwise = fib(x-1) + fib(x-2)



resto :: Integer -> Integer -> Integer
resto x 0	= error "El dividendo no puede ser 0."
resto x y	| x < y = 0
resto x y	= resto (x-y) y
	
resto' :: Integer -> Integer -> Integer
resto' x y	| y == 0 = error "El dividendo no puede ser 0."
			| x >= y = resto (x-y) y
			| x < y  = x



cociente :: Integer -> Integer -> Integer
cociente x y | y == 0 = error "El dividendo no puede ser 0."
			 | x >= y = (cociente (x-y) y) + 1		{- 1 + cociente (x-y) y -}
			 | x < y  = 0

p4 x y =	x > 0 && y > 0	==> cociente x y == div x y



cocienteResto :: Integer -> Integer -> (Integer, Integer)
{- cocienteResto x y =  (cociente x y, resto x y) consume mucho cómputo-}
cocienteResto x y | y == 0 = error "El dividendo no puede ser 0."
				  | x >= y = tuplear (1,0) (cocienteResto(x-y) y, y)
				  | x < y  = (0, x)
	where tuplear (a1,a2) (b1,b2) = (a1+b1 , a2+b2)
	
	
cocienteResto' :: 
cocienteResto' x y | x < y = (0,x)
				   | otherwise = (u+1,v)
	where (u,v) = (cocienteResto (x-y) y, v)