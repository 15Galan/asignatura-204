-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación:			Grado en Ingeniería [Informática].
-- Alumno:				GALÁN HERRERA, ANTONIO J.
-- Fecha de entrega:	DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: TODOS. 
-- 
-------------------------------------------------------------------------------

import Test.QuickCheck



-------------------------------------------------------------------------------
-- Ejercicio 1
-------------------------------------------------------------------------------

esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = x^2 + y^2 == z^2

terna :: Integer -> Integer -> (Integer,Integer,Integer)
terna x y = (x^2 - y^2, 2*x*y, x^2 + y^2)

p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h
  where
    (l1,l2,h) = terna x y
	
-- Para todos los valores positivos de X e Y, siendo a su vez X mayor que Y,
-- se cumple que los valores L1 (x), L2 (y) y H forman una terna pitagórica.

-- *Main> quickCheck p_ternas
-- *** Gave up! Passed only 77 tests.



-------------------------------------------------------------------------------
-- Ejercicio 2
-------------------------------------------------------------------------------

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)



-------------------------------------------------------------------------------
-- Ejercicio 3
-------------------------------------------------------------------------------
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y)	| x > y  = (y,x)
				| y >= x = (x,y)

p1_ordena2 x y = enOrden (ordena2 (x,y))
	where
		enOrden (x,y) = x<=y

-- La propiedad "p1_ordena2" establece que un par está ordenado "enOrden" si el par de
-- la función "ordena2" viene dado por las variables ordenadas de menor a mayor.
	
p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
	where
		mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)				
--		mismosElementos (x,y) (z,v) = (x,y) == (z,v) || (x,y) == (v,z)

-- La propiedad "p2_ordena2" establece que un par de pares de elementos (x,y) y (z,v) contienen los
-- mismos elementos si los valores del primer par son los mismos que los del segundo.
	
ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) | y < x 	= ordena3 (y,x,z)
				| z < y 	= ordena3 (x,z,y)
				| z < x		= ordena3 (z,y,x)
				| otherwise = (x,y,z)

p1_ordena3 x y z = enOrden (ordena3(x,y,z))
	where
		enOrden (x,y,z) = x<=y && y<=z

-- La propiedad "p1_ordena3" establece que un trío está ordenado "enOrden" si el trío resultante
-- de la función "ordena3" viene dado por las variables ordenadas de menor a mayor (x < y < z).

p2_ordena3 x y z = mismosElementos (x,y,z) (ordena3 (x,y,z))
	where
		mismosElementos (x,y,z) (t,v,w) = (x,y,z) == (t,v,w) || (x,y,z) == (v,t,w) || (x,y,z) == (v,w,t) || (x,y,z) == (t,w,v) || (x,y,z) == (w,v,t)

-- La propiedad "p2_ordena3" establece que un par de tríos de elementos (x,y,z) y (t,v,w) contienen los
-- mismos elementos si los valores del primer trío son los mismos que del segundo.



-------------------------------------------------------------------------------
-- Ejercicio 4
-------------------------------------------------------------------------------

max2 :: Ord a => a -> a -> a
max2 x y | x >= y	 = x
		 | otherwise = y

p1_max2 x y = m == x || m == y
	where
		m = max2 x y
		
p2_max2 x y = mm >= x || mm >= y
	where
		mm = max2 x y
		
p3_max2 x y = x >= y ==> x == mx
	where
		mx = max2 x y
		
p4_max2 x y = y >= x ==> y == mxm
	where
		mxm = max2 x y



-------------------------------------------------------------------------------
-- Ejercicio 5
-------------------------------------------------------------------------------

entre :: Ord a => a -> (a, a) -> Bool
entre x (p,q) = (p <= x && x <= q)



-------------------------------------------------------------------------------
-- Ejercicio 6
-------------------------------------------------------------------------------

iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z) = (x == y && y == z)



-------------------------------------------------------------------------------
-- Ejercicio 7
-------------------------------------------------------------------------------

-- Para este ejercicio nos interesa utilizar la función predefinida en Prelude: 
-- 		divMod :: Integral a => a -> a -> (a, a)
-- que calcula simultáneamente el cociente y el resto:
--
--   *Main> divMod 30 7
--   (4,2)

type TotalSegundos = Integer
type Horas         = Integer
type Minutos       = Integer
type Segundos      = Integer

descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas, minutos, segundos)
   where
	(horas,resto)		= divMod x 3600
	(minutos,segundos)	= divMod resto 60
	
	
p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x
                           && m `entre` (0,59)
                           && s `entre` (0,59)
	where
		(h,m,s) = descomponer x
		
-- *Main> quickCheck p_descomponer
-- +++ OK, passed 100 tests.
         


-------------------------------------------------------------------------------
-- Ejercicio 8
-------------------------------------------------------------------------------

unEuro :: Double
unEuro = 166.386

pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro

eurosAPesetas :: Double -> Double
eurosAPesetas x = x*unEuro

p_inversas x = eurosAPesetas (pesetasAEuros x) == x

-- La propiedad no se verifica ya que en el proceso se pierden decimales
-- al trabajar ambas funciones en el entorno "Double -> Double".



-------------------------------------------------------------------------------
-- Ejercicio 9
-------------------------------------------------------------------------------

infix 4 ~=
(~=) :: Double -> Double -> Bool

x ~= y = abs (x-y) < epsilon
	where
		epsilon = 1/1000

p2_inversas x = eurosAPesetas (pesetasAEuros x) ~= x

-------------------------------------------------------------------------------
-- Ejercicio 10
-------------------------------------------------------------------------------

-- Usaremos el operador =~ visto en clase (Tema 1, transparencia 57)
infix 0 =~
(=~) :: (Ord a, Fractional a) => a -> a -> Bool

x =~ y = abs (x-y) < epsilon
	where
		epsilon = 1e-5

-- Primera solución
-- no consideramos el estudio de las raices para a=0, 
raíces :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raíces a b c 	| dis < 0     = error "Raíces no reales"
				| otherwise   = (resA, resB)
	where
		dis  = b^2 - 4*a*c
		resA = (-b + sqrt dis)/2*a
		resB = (-b - sqrt dis)/2*a


p1_raíces a b c  = True   ==> esRaíz r1 && esRaíz r2
-- atención, en el caso de True, podemos eliminar:  True ==> 
	where
		(r1,r2) = raíces a b c
		esRaíz r = a*r^2 + b*r + c =~ 0

p2_raíces a b c  = (a /= 0) && (b^2 - 4*a*c >= 0) ==> esRaíz r1 && esRaíz r2
	where
		(r1,r2) = raíces a b c
		esRaíz r = a*r^2 + b*r + c =~ 0



-------------------------------------------------------------------------------
-- Ejercicio 11
-------------------------------------------------------------------------------

esMúltiplo :: Integer -> Integer -> Bool
esMúltiplo x y = x `mod` y == 0



-------------------------------------------------------------------------------
-- Ejercicio 12
-------------------------------------------------------------------------------

infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
False ==>> y = True
True  ==>> y = False



-------------------------------------------------------------------------------
-- Ejercicio 13
-------------------------------------------------------------------------------

esBisiesto :: Integer -> Bool
esBisiesto x	| (x `esMúltiplo` 100) = x `esMúltiplo` 400
				| (x `esMúltiplo` 4) = True
				| otherwise = False

p_esBisiesto x = x `esMúltiplo` 100 ==> x `esMúltiplo` 400


{-
Otro modo (clase):

esBisiesto :: Integer -> Bool
esBisiesto x | x `esMúltiplo` 4 && (x `esMúltiplo` 100 ==> x `esMúltiplo` 400)
-}



-------------------------------------------------------------------------------
-- Ejercicio 14
-------------------------------------------------------------------------------

-- potencia con base un número arbitrario
potencia :: (Num b, Integral n) => b -> n -> b
potencia b 0				= 1
potencia b n	| n > 0		= b*(potencia b (n-1))
				| otherwise = error "Potencia negativa"


potencia' :: (Num b, Integral n) => b -> n -> b
potencia' b 0					 = 1
potencia' b 2					 = b*b
potencia' b n	| n `mod` 2 == 0 = potencia' (potencia' b (div n 2)) 2
				| n `mod` 2 /= 0 = b * potencia' (potencia' b (div (n-1) 2)) 2
				| otherwise		 = error "Potencia negativa"


-- con esta propiedad (BASE un entero) no hay problemas
p_pot :: Integer -> Integer -> Property
p_pot b n  = n>=0 ==> (potencia b n == sol) && (potencia' b n == sol)
	where
		sol = b^n
   
-- *Main> quickCheck p_pot
-- +++ OK, passed 100 tests.

{-
-- SEGUNDA OPCION: si consideramos una base arbitraria hay muchos problemas
p_pot' :: (Ord b, Fractional b, Integral n) => b -> n -> Property
p_pot' b n  = n>=0 ==> (potencia b n ~= sol) && (potencia' b n ~= sol)
	where
		sol = b^n
   
-- *Main> quickCheck p_pot'
-- *** Failed! Falsifiable (after 7 tests and 1 shrink):  
-- 4.374147831506856
-- 4   

-- Main> potencia 850.1 5 - 850.1^5
-- 6.25e-2

-- Debemos ~= por un concepto de error relativo
-}


-------------------------------------------------------------------------------
-- Ejercicio 15
-------------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial x	| x == 1	= x
			| otherwise = x * factorial (x-1)



-------------------------------------------------------------------------------
-- Ejercicio 16
-------------------------------------------------------------------------------

divideA :: Integral a => a -> a -> Bool
divideA n m	| n /= 0 	 = mod m n == 0
			| otherwise  = error "División por 0"


p1_divideA x y	 = y/=0 && y `divideA` x ==> div x y * y == x

p2_divideA x y z = z/=0 && z `divideA` x && z `divideA`y ==> z `divideA` (x+y)


-- par = divideA 2
-- esMúltiploDe5 = divideA 5



-------------------------------------------------------------------------------
-- Ejercicio 17
-------------------------------------------------------------------------------

mediana :: Ord a => (a, a, a, a, a) -> a
mediana (x,y,z,t,u) | x > z = mediana(z,y,x,t,u)
					| y > z = mediana(x,z,y,t,u)
					| z > t = mediana(x,y,t,z,u)
					| z > u = mediana(x,y,u,t,z)
					| otherwise = z



-------------------------------------------------------------------------------
-- Ejercicio Extra
-------------------------------------------------------------------------------

-- Diseña una función que devuelva un par con el cociente y el resto de
-- una división, utilizando solo sumas y restas en su desarrollo.

cocienteYresto :: Integer -> Integer -> (Integer, Integer)
cocienteYresto x y	| x < y = (0,x)
					| otherwise = (c+1,r)
	where
		(c,r) = cocienteYresto (x-y) y


{-
Apuntes de clase (final).

f :: a -> (a -> b) -> (a -> b -> c) -> c			[x :: a]
f x g h = h x (g x)

f' :: (b -> b -> c) -> a -> (a -> b) -> c			[x :: a]
f' g x h = g (h x) (h x)

f` :: (b -> a -> c) -> a -> (a -> b) -> c		"Tautología en la lógica de proposición (porque tiene habitantes)"
f` g x h = g (h x) x							"Habitante del tipo"


f´ :: (Ord a, Integer b) => a -> a -> b -> Bool
f´ x y z	| x < y		= div z 2 == 0
			| otherwise = false
-}