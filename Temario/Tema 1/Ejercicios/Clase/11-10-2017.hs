-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería [Informática].
-- Alumno: GALÁN HERRERA, ANTONIO J.
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: 3 / 17
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