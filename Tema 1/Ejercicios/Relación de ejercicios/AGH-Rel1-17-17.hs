-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería [Informática].
-- Alumno: GALÁN HERRERA, ANTONIO J.
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: 17 / 17
-------------------------------------------------------------------------------
import Test.QuickCheck


---------------------------------------
-- Ejercicio 1
---------------------------------------

-- a)
esTerna  :: Integer -> Integer -> Integer -> Bool
esTerna x y z = (x*x + y*y == z*z)


-- b)
terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y | (x > 0) && (y > 0) && (x > y) = (x*x-y*y, 2*x*y, x*x+y*y)


-- c) y d)
p_ternas x y = (x > 0) && (y > 0) && (x > y) ==> esTerna l1 l2 h
    where
        (l1,l2,h) = terna x y



---------------------------------------
-- Ejercicio 2
---------------------------------------

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)



---------------------------------------
-- Ejercicio 3
---------------------------------------

-- a)
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x,y) | x < y     = (y,x)
              | otherwise = (x,y)

p1_ordena2 x y = enOrden (ordena2 (x,y))
    where
        enOrden (x,y) = x <= y

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
    where
        mismosElementos (x,y) (z,v) = (x == z && y == v)
                                   || (x == v && y == z)


-- b)
ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) | x > y     = ordena3(y,x,z)
                | y > z     = ordena3(x,z,y)
                | x > z     = ordena3(z,y,x)
                | otherwise = (x,y,z)


-- c)
p1_ordena3 x y z = enOrden (ordena3 (x,y,z))
    where
        enOrden (x,y,z) = (x <= y) && (y <= z)

p2_ordena3 x y z = mismosElementos (x,y,z) (ordena3 (x,y,z))
    where
        mismosElementos (x,y,z) (r,s,t) = (x==r) && (y==s) && (z==t)
                                       || (x==s) && (y==r) && (z==t)
                                       || (x==r) && (y==t) && (z==s)
                                       || (x==t) && (y==s) && (z==r)



---------------------------------------
-- Ejercicio 4
---------------------------------------

-- a)
max2 :: Ord a => a -> a -> a
max2 x y | x >= y    = x
         | otherwise = y


-- b)
p1_max2 x y = (max2 x y == x) || (max2 x y == y)

p2_max2 x y = (max2 x y >= x) || (max2 x y >= y)

p3_max2 x y = (x >= y) ==> max2 x y

p4_max2 x y = (y >= x) ==> max2 x y



---------------------------------------
-- Ejercicio 5
---------------------------------------

entre :: Ord a => a -> (a,a) -> Bool
entre z (x,y) | (z >= x) && (z <= y) = True     -- Uso ">=" porque el enunciado 
              | otherwise            = False    -- dice "[Min,Max]", ambos incluidos



---------------------------------------
-- Ejercicio 6
---------------------------------------

iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z) = (x == y && y == z)



---------------------------------------
-- Ejercicio 7
---------------------------------------

-- a)
type Horas         = Integer
type Minutos       = Integer
type Segundos      = Integer
type TotalSegundos = Integer

descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas, minutos, segundos)
    where
        horas    = x `div` 3600
        resto    = x `mod` 3600
        minutos  = resto `div` 60
        segundos = resto `mod` 60


-- b)
p_descomponer x = x >= 0 ==> h*3600 + m*60 + s == x
                             && entre m (0,59)
                             && entre s (0,59)
    where
        (h,m,s) = descomponer x



---------------------------------------
-- Ejercicio 8
---------------------------------------

unEuro :: Double
unEuro = 166.386

-- a)
pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro


-- b)
eurosAPesetas :: Double -> Double
eurosAPesetas x = x*unEuro


-- c)
p_inversas x = eurosAPesetas (pesetasAEuros x) == x

-- La propiedad no se verifica, ya que en el proceso se pierden decimales
-- al trabajar ambas funciones en el entorno "Double -> Double".



---------------------------------------
-- Ejercicio 9
---------------------------------------

infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where
        epsilon = 1/1000

p2_inversas x = eurosAPesetas (pesetasAEuros x) ~= x



---------------------------------------
-- Ejercicio 10
---------------------------------------

-- a) 
raíces :: (Ord a, Floating a) => a -> a -> a -> (a,a)
raíces a b c | dis >= 0  = (r1, r2)
             | otherwise = error "Raíces no reales"
    where
        dis = b^2 - 4*a*c
        r1  = (-b + sqrt dis)/(2*a)        -- Importante usar paréntesis tanto en el numerador como
        r2  = (-b - sqrt dis)/(2*a)        -- en el denominador, para no hacer "numerador/2 * a"


-- b)
p1_raíces a b c = esRaíz r1 && esRaíz r2
    where
        (r1,r2) = raíces a b c
        esRaíz r = a*r^2 + b*r + c ~= 0


p2_raíces a b c  = (a /= 0) && (b^2 - 4*a*c >= 0) ==> esRaíz r1 && esRaíz r2
-- p2_raíces a b c = (r1 + r2 = -b/a) && (r1 * r2 = c/a) ==> esRaíz r1 && esRaíz r2
    where
        (r1,r2)  = raíces a b c
        esRaíz r = a*r^2 + b*r + c ~= 0



---------------------------------------
-- Ejercicio 11
---------------------------------------

esMúltiplo :: Integer -> Integer -> Bool
esMúltiplo x y = x `mod` y == 0



---------------------------------------
-- Ejercicio 12
---------------------------------------

infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
False ==>> y     = True
True  ==>> True  = True
True  ==>> False = False



---------------------------------------
-- Ejercicio 13
---------------------------------------

esBisiesto :: Integer -> Bool
esBisiesto x | m4        = m100 ==>> m400
             | otherwise = False
    where
        m4   = x `esMúltiplo` 4
        m100 = x `esMúltiplo` 100
        m400 = x `esMúltiplo` 400

p_esBisiesto x = x `esMúltiplo` 100 ==> x `esMúltiplo` 400



---------------------------------------
-- Ejercicio 14
---------------------------------------

-- a)
potencia :: (Num a, Integral a) => a -> a -> a
potencia b 0             = 1
potencia b n | n > 0     = b * (potencia b (n-1))
             | otherwise = error "Exponente negativo"


-- b)
potencia' :: (Num a, Integral a) => a -> a -> a
potencia' b 0             = 1
potencia' b 2             = b*b                                            -- Bucle infinito en "potencia' _ 2"
potencia' b n | par       = potencia' (potencia' b (div n 2)) 2            -- Necesitas el cociente, no "n/2"
              | impar     = b * potencia' (potencia' b (div (n-1) 2)) 2
              | otherwise = error "Exponente negativo"
    where
        par   = n `mod` 2 == 0
        impar = not par


-- c)
p_pot :: Integer -> Integer -> Property
p_pot b n = n >= 0 ==> potencia b n == sol && potencia' b n == sol
    where
        sol = b^n


-- d)                                                                        -- TERMINAR
-- eficiencia :: (Num a, Integral a) => (a -> b) -> a -> a
-- eficiencia f c = f


---------------------------------------
-- Ejercicio 15
---------------------------------------

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)



---------------------------------------
-- Ejercicio 16
---------------------------------------

-- a)
divideA :: Integral a => a -> a -> Bool
divideA n m | n /= 0    = (mod m n == 0)
            | otherwise = error "División por 0"


--- b)
p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x


-- c)
p2_divideA x y z = z/=0 && z `divideA` x && z `divideA`y ==> z `divideA` (x+y)



---------------------------------------
-- Ejercicio 17
---------------------------------------

mediana :: Ord a => (a, a, a, a, a) -> a            -- En realidad es como un "ordena5"
mediana (x,y,z,t,u) | x > z = mediana(z,y,x,t,u)    -- del ejercicio 3, pero devolviendo
                    | y > z = mediana(x,z,y,t,u)    -- el valor central de la tupla
                    | t < z = mediana(x,y,t,z,u)
                    | u < z = mediana(x,y,u,t,z)
                    | otherwise = z
