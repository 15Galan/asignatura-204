-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería [Informática].
-- Alumno: GALÁN HERRERA, ANTONIO J.
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1 (extra). Ejercicios resueltos: 3 / 5.
-------------------------------------------------------------------------------
import Test.QuickCheck


---------------------------------------
-- Ejercicio [esPrimo]
---------------------------------------

esPrimo :: (Num a, Integral a) => a -> Bool
esPrimo x | x <= 0                     = error "Argumento negativo o cero"
          | x == 2 || x == 3 || x == 5 = True
          | not m2 && not m3 && not m5 = True
          | otherwise                  = False
    where
        m2 = (x `mod` 2 == 0)
        m3 = (x `mod` 3 == 0)
        m5 = (x `mod` 5 == 0)



---------------------------------------
-- Ejercicio [libreDeCuadrados]
---------------------------------------

libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n | n <= 0                     = error "Argumento negativo o cero"
                   | n == 2 || n == 3 || n == 5 = True
                   | not m2 && not m3 && not m5 = True
                   | otherwise                  = False
    where
        m2 = (n `mod` 2^2 == 0)
        m3 = (n `mod` 3^2 == 0)
        m5 = (n `mod` 5^2 == 0)



---------------------------------------
-- Ejercicio [Harshad]
---------------------------------------

sumaDigitos :: Integer -> Integer
sumaDigitos x | x < 0     = error "Argumento negativo"
              | x < 10    = x
              | otherwise = (mod x 10) + sumaDigitos (div x 10)


harshad :: Integer -> Bool
harshad 0 = error "Argumento no positivo"
harshad x = x `mod` (sumaDigitos x) == 0            -- La subfunción "sumaDigitos" ya contempla la excepción


multipleHarshad :: Integer -> Bool              -- La función de los ejemplos no es "multipleHashard" (errata)
multipleHarshad x = harshad x && harshad (div x (sumaDigitos x))


vecesHarshad :: Integer -> Integer                  -- NO FUNCIONA
vecesHarshad 1 = 1
vecesHarshad x | x <= 0            = error "Argumento no positivo"
               | multipleHarshad x = 1 + vecesHarshad num
               | otherwise         = 0
    where
        num = div x (sumaDigitos x)



---------------------------------------
-- Ejercicio [cerosDe]
---------------------------------------

cerosDe :: Integer -> Integer
cerosDe x | x `mod` 10 == 0 = 1 + cerosDe (x `div` 10)
          | otherwise       = 0


prop_cerosDe_OK n m = m >= 0 && m <= 1000 ==> cerosDe n*m == cerosDe m    -- ERROR



---------------------------------------
-- Ejercicio [Fibonacci]
---------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n | n > 1     = fib (n-1) + fib (n-2)
      | otherwise = error "Argumento negativo"


llamadasFib :: Integer -> Integer
llamadasFib n | n > 1  = 1 + llamadasFib (n-1) + llamadasFib (n-2)
              | n >= 0 = 1


-- Para "fib 30" se obtienen 2692537 llamadas.
-- Para "fib 60" se obtienen 5385074 llamadas.


fib' :: Integer -> Integer
fib' n = fibAc n 0 1
    where
        fibAc 1 i j = j
        fibAc n i j = fibAc (n-1) j (i+j)                -- Implementación "chapuza"
--      fibAc 0 i j = i + j
--      fibAc n i j = fibAc (n-1) (fib (i+1)) (fib (j+1))

-- Para "fib' 30" se realizan 30 llamadas.
-- Para "fib' 60" se realizan 60 llamadas.


binet :: Integer -> Integer
-- binet x = div (oro^x - (1 - oro)^x) r5
binet x = div ((div (1+r5) 2)^x - (div (1-r5) 2)^x) r5   -- Devuelve 0 en los pares y 1 en los impares
    where
        r5  = round (sqrt 5)
        oro = div (1 + r5) 2











