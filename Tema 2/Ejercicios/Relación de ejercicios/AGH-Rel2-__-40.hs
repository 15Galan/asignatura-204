-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería [Informática].
-- Alumno: GALÁN HERRERA, ANTONIO J.
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: 07 / 40
-------------------------------------------------------------------------------
import Test.QuickCheck
import Data.List


---------------------------------------
-- Ejercicio 1                         -- INCOMPLETO
---------------------------------------

data Direction = North | South | East | West deriving (Eq,Ord,Enum,Show)

{-
-- a)
(<<) :: Direction -> Direction -> Bool

p_menor x y = (x < y) == (x << y)
instance Arbitrary Direction where
    arbitrary = do
        n <- choose (0,3)
        return $ toEnum n


-- b)
data Direction = North | South | East | West deriving (Eq,Enum,Show)
-}


---------------------------------------
-- Ejercicio 2
---------------------------------------

-- a)
máximoYresto :: Ord a => [a] -> (a,[a])
máximoYresto [ ] = error "Lista vacía"
máximoYresto [x] = (x,[ ])
máximoYresto (x:xs) | x > max   = (x, max:res)
                    | otherwise = (max, x:res)
    where
        (max,res) = máximoYresto xs


-- b)
máximoYresto' :: Ord a => [a] -> (a,[a])
máximoYresto' [ ] = error "Lista vacía"
máximoYresto' [x] = (x,[ ])
máximoYresto' (x:xs) | x > max   = (x, (sort (max:res)))
                     | otherwise = (max, (sort (x:res)))
    where
        (max,res) = máximoYresto' xs



---------------------------------------
-- Ejercicio 3
---------------------------------------

reparte :: [a] -> ([a],[a])
reparte [ ] = ([ ],[ ])
reparte [x] = ([x],[ ])
reparte (x:y:res) = (x:xs,y:ys)
    where
        (xs,ys) = reparte res            -- Supones el último caso



---------------------------------------
-- Ejercicio 4
---------------------------------------

distintos :: Ord a => [a] -> Bool
distintos [ ] = error "No hay elementos."
distintos [x] = True
distintos (x:xs) | repetido == False = distintos xs
                 | otherwise                           = False
    where
        repetido = or $ map (==x) xs

{-
distintos' :: Ord a => [a] -> Bool
distintos' [ ] = error "Lista vacía."
distintos' x   = False
distintos' (x:xs) | (length $ takeWhile (==x) lista) == 1 = distintos xs
                  | otherwise                           = False
    where
        lista = sort (x:xs)

    -- ERROR, NO FUNCIONA (supongo que por poco)
-}



---------------------------------------
-- Ejercicio 5
---------------------------------------

-- a)
replicate' :: Int -> a -> [a]
replicate' 0 x = [ ]
replicate' 1 x = [x]
replicate' n x = x : replicate (n-1) x


-- b)
p_replicate' n x = n >= 0 && n <= 1000 ==> length (filter (==x) xs) == n
                                           && length (filter (/=x) xs) == 0
   where xs = replicate' n x

    -- Comprueba que el número de elementos de la lista es el valor n
    -- y que todos los elementos de la lista son el mismo valor x.


-- c)
    -- +++ OK, passed 100 tests.



---------------------------------------
-- Ejercicio 6
---------------------------------------

divisores :: Integer -> [Integer]                       -- La función daba error, porque "mod" necesita
divisores 0 = error "El número 0 no tiene divisores."   -- 2 argumentos. Creé la función parcial en "y"
divisores 1 = [1]                                       -- para que "y" fuese el elemento que recibe de
divisores x = filter (\y -> mod x y == 0) [1..x]        -- la lista, siendo este variable (no como "x"). 


divisores' :: Integer -> [Integer]
divisores' 0 = error "El número 0 no tiene divisores."
divisores' 1 = [1]
divisores' x = filter (\y -> mod x y == 0) ([(-x)..(-1)] ++ [1..10])



--------------------------------------
-- Ejercicio 7
--------------------------------------

-- a)
mcd :: Integer -> Integer -> Integer
mcd 0 y = y
mcd x 0 = x
mcd x y | (x<y) || (x>y) = maximum divisoresComunes
        | otherwise      = x
    where
        divisoresComunes = [if (n `elem` (divisores x)) then n else 0 | n <- (divisores y)]


-- b)
p_mcd x y z = (x>0) && (y>0) && (z>0) ==> mcd (x*z) (y*z) == abs z * (mcd x y)
    -- +++ OK, passed 100 tests.


-- c)
mcm :: Integer -> Integer -> Integer
mcm x y = div (x*y) (mcd x y)



--------------------------------------
-- Ejercicio 8
--------------------------------------

-- a)
-- Copiada de en la Relación de Ejercicios (extra) del Tema 1.
esPrimo :: (Num a, Integral a) => a -> Bool
esPrimo x | x <= 0         = error "Argumento negativo o cero"
          | p2 && p3 && p5 = True
          | otherwise      = False
    where
        p2 = not (x `mod` 2 == 0) || x == 2
        p3 = not (x `mod` 3 == 0) || x == 3
        p5 = not (x `mod` 5 == 0) || x == 5


-- b)
primosHasta :: Integer -> [Integer]
primosHasta 0 = [ ]
primosHasta 1 = [ ]
primosHasta x = filter (esPrimo) [1..x]



--------------------------------------
-- Ejercicio 9
--------------------------------------
pares :: Integer -> [(Integer,Integer)]
pares n | n < 2     = [ ]
        | otherwise = [ (x,y) | x <- (primosHasta (div n 2)), y <- (primosHasta n), x+y == n]

    -- Deducción: Los pares empezarán a repetirse cuando pasen la mitad de los
    -- valores de la lista de "primosHAsta n", por lo que una de las sublistas
    -- debe recorreserse solo hasta la mitad, de forma que no aparecerán pares
    -- repetidos como (3,7) y (7,3) para n = 10, ya que (5,5) es el par central.
    -- Usa la demo para ver un ejemplo de la lista por comprensión base del ejercicio.

    --demo n = [ (x,y) | x <- (primosHasta (div n 2)), y <- (primosHasta n)]



--------------------------------------
-- Ejercicio 10
--------------------------------------

-- a)
esPerfecto :: Integer -> Bool
esPerfecto 0 = error "El cero no es divisor de ningún elementos"
esPerfecto x = sum (divisores x) - x == x        -- Hay que restar "x" para que no se
                                                 -- cuente a sí mismo como divisor.


-- b)
perfectosMenoresQue :: Integer -> [Integer]
perfectosMenoresQue 0 = [ ]
perfectosMenoresQue x = filter (esPerfecto) [1..x]



--------------------------------------
-- Ejercicio 11
--------------------------------------

-- a)
take' :: Int -> [a] -> [a]
take' n xs = [ x | (p,x) <- zip [0..(n-1)] xs ]


-- b)
drop' :: Int -> [a] -> [a]
drop' n xs = [ x | (p,x) <- zip [1..(length xs)] xs,  p>n ]


-- c)
p_take'drop' n xs = n>=0 ==> ((take' n xs) ++ (drop' n xs)) == xs
    -- +++ OK, passed 100 tests.



--------------------------------------
-- Ejercicio 12   *
--------------------------------------

-- a)
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs


-- b)
concat'' :: [[a]] -> [a]
concat'' [ ] = [ ]
concat'' (x:xs) = generador2 ++ concat'' xs
    where
        generador1 = [ l | l <- x]
        generador2 = [ n | n <- generador1]

--------------------------------------
-- Ejercicio 13
--------------------------------------

desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]

    -- La función "desconocida" genera una lista de tuplas mediante la función "zip", donde el
    -- primer valor de la tupla pertenece a la lista pasada como argumento en "desconocida" y el
    -- segundo valor pertence a la cola que resulta de la misma lista.
    -- Siempre resulta una lista de tuplas donde cada una de ellas se cumple que:
    -- el primer argumento es un valor de la lista original y el segundo valor es el siguiente
    -- valor en dicha fila.
    -- A continuación, dichas tuplas pasan a ser valores booleanos, por lo que devuelve un dato
    -- del tipo [Bool] basado en el "x<=y" en la definición de la lista por compresnsión.
    -- Finalmente, añade la función "and" que devuelve el resultado booleano de ejecutar un "and"
    -- entre todos los valores de la lista.



--------------------------------------
-- Ejercicio 14
--------------------------------------

-- a)
inserta :: (Ord a) => a -> [a] -> [a]
inserta x xs = l1 ++ [x] ++ l2
    where
        l1 = takeWhile (<=x) xs
        l2 = dropWhile (<=x) xs


-- b)
inserta' :: (Ord a) => a -> [a] -> [a]
inserta' x [ ]         = [x]
inserta' x xs | x <= l = x:xs
              | x > l  = l : inserta' x ls
    where
        (l:ls) = xs


-- c)
p1_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

    -- +++ OK, passed 100 tests.

    -- La propiedad indica que si una lista "xs" está ordenada, al introducir
    -- un número con la función "inserta", esta devolverá una lista también ordenada.


-- d)
    -- Funciona porque la función inserta introduce un valor en una lista de forma ordenada,
    -- sea cual sea la lista. Por lo que, al introducir el primer valor, resultará una lista
    -- ordenada; al introducir el segundo, lo insertará de forma ordenada, resultando de nuevo
    -- una lista ordenada, y así sucesivamente en cada paso.
    -- La función "inserta" produce una lista ordenada, por lo que cada uso de la misma
    -- producirá una lista ordenada y en su conjunto, resultará una lista ordenada final.


-- e)
ordena :: (Ord a) => [a] -> [a]
ordena xs = foldr (inserta) [] xs


-- f)                                                                                              *
p_ordena xs = desconocida (ordena xs) ==> True

    -- Interrupted


-- g)                                                                                              *
{-
    Caso base: "desconocida (ordena []) == True".
    Efectivamente, se cumple.

    ordena [] = []
    desconocida [] = True


    Caso inductivo: "desconocida (ordena xs) == True".
	Se cumple.
	
	ordena xs =
-}



--------------------------------------
-- Ejercicio 15
--------------------------------------

-- a)
geométrica :: Integer -> Integer -> [Integer]
geométrica x r = x : geométrica (x*r) r


-- b)
p1_geométrica x r = x>0 && r>0 ==> and [ div z y == r | (y,z) <- zip xs (tail xs) ]
    where
        xs = take 100 (geométrica x r)

    -- La función por comprensión de la propiedad es similar a la vista anteriormente
    -- en el Ejercicio 13, salvo en la expresión, que ahora comprueba que efectivamente
    -- el siguiente valor de la lista es el valor anterior con la razón aplicada.
    -- Es decir, comprueba que el valor posterior es la razón multiplicada por el anterior.
    -- Finalmente, devuelve True si todas las comprobaciones resultan correctas, lo que
    -- demuestra que la secuencia es geométrica.


-- c)
múltiplosDe' :: Integer -> [Integer]
múltiplosDe' 0 = [0]
múltiplosDe' x = iterate (+x) 0


-- d)
potenciasDe :: Integer -> [Integer]
potenciasDe 0 = [0]
potenciasDe x = iterate (*x) 1



---------------------------------------
-- Ejercicio 16
---------------------------------------

-- a)
múltiplosDe :: Integer -> [Integer]
múltiplosDe 0 = [0]
múltiplosDe x | x > 0     = iterate (+x) x
              | otherwise = error "Valor negativo."


-- b)
primeroComún :: Ord a => [a] -> [a] -> a
-- primeroComún (x:xs) [ ] = x
-- primeroComún [ ] (y:ys) = y
primeroComún (x:xs) (y:ys) | x == y    = x
                           | otherwise = n
    where
        ((n,m):ls) = [ (p,q) | p <- (x:xs), q <- (y:ys), p == q]

            -- Idea (mía): Generar una lista de tuplas, donde las tuplas
            -- generadas son todas las combinaciones (en orden) de
            -- los datos de ambas listas. Una vez obtenida esa lista
            -- de tuplas, filtrarla para conseguir una lista cuyas
            -- tuplas sean con el primer y el segundo valor idénticos.
            -- Finalmente, devolver la primera tupla de esa lista.
            -- Con la lista por comprensión ya filtro las tuplas con
            -- valores iguales, de forma que lo único que tengo que
            -- devolver es la primera tupla (primer común), de ahí el
            -- "((n,m):ls)" en lugar de "l = (n,m)" y "(l:ls) = [(p,q) ...".


-- c)
mcm' :: Integer -> Integer -> Integer
mcm' x y = primeroComún (múltiplosDe x) (múltiplosDe y)



-- d)                                                                                              *
p_mcm x y = x>=0 && y>=0 ==> mcm x y == lcm x y



--------------------------------------
-- Ejercicio 17
--------------------------------------

-- a)
primeroComúnDeTres :: Ord a => [a] -> [a] -> [a] -> a
-- primeroComúnDeTres [ ] (y:ys) (z:zs) = primeroComún (y:ys) (z:zs)
-- primeroComúnDeTres (x:xs) [ ] (z:zs) = primeroComún (x:xs) (z:zs)
-- primeroComúnDeTres (x:xs) (y:ys) [ ] = primeroComún (x:xs) (y:ys)
-- Faltarían los casos en los que recibe 2 listas vacías.
primeroComúnDeTres (x:xs) (y:ys) (z:zs) | x > y     = primeroComúnDeTres (x:xs) ys (z:zs)
                                        | y > z     = primeroComúnDeTres (x:xs) (y:ys) zs
                                        | z > x     = primeroComúnDeTres xs (y:ys) (z:zs)
                                        | iguales   = x
                                        | otherwise = error "No hay ningún elemento común."
    where
        iguales = and [x == y, x == z, y == z]

            -- Idea (profesor): Descartar de las listas los valores menores al
            -- primer valor común de las 3 listas.
            -- Comprobando que el primer elemento de la lista es menor con respecto 
            -- a otro y sabiendo que las listas están ordenadas ascendentemente, al
            -- descartar todos los elementos menores anteriores al primer común,resultará
            -- que el primer elemento de cada lista será dicho elemento común.



--------------------------------------
-- Ejercicio 18
--------------------------------------

factPrimos :: Integer -> [Integer]
factPrimos x = fp x 2
    where
        fp x d | x' < d = [x]
               | r == 0 = d : fp x' d
               | otherwise = fp x (d+1)
            where
                (x',r) = divMod x d        -- cociente y resto


-- a)
{-

Valor entre paréntesis -> Valor actualizado con respecto al paso anterior.

-- Paso #1:
factPrimos :: Integer -> [Integer]
factPrimos (220) = fp (220) 2
    where
        fp (220) (2) | False     = . . .
                     | (0) == 0  = (2) : fp (110) (2)
                     | otherwise = . . .
            where
                (110,0) = divMod (220) (2)

    ** Se obtiene "(2) : ...".


-- Paso #2:
        fp (110) 2 | False     = . . .
                   | (0) == 0  = 2 : fp (55) (2)
                   | otherwise = . . .
            where
                (55,0) = divMod (110) 2

    ** Se obtiene "2 : (2) : ...".


-- Paso #3:
        fp (55) 2 | False     = . . .
                  | False     = . . .
                  | otherwise = fp 55 (3)
            where
                (27,1) = divMod (55) 2

    ** Se obtiene "2 : 2 : fp (55) (3)".


-- Paso #4:
        fp 55 (3) | False     = . . .
                  | False     = . . .
                  | otherwise = fp 55 (4)
            where
                (18,1) = divMod 55 (3)

    ** Se obtiene "2 : 2 : fp 55 (4)".


-- Paso #5:
        fp 55 (4) | False     = . . .
                  | False     = . . .
                  | otherwise = fp 55 (5)
            where
                (13,3) = divMod 55 (4)

    ** Se obtiene "2 : 2 : fp 55 (5)".


-- Paso #6:
        fp 55 (5) | False     = . . .
                  | (0) == 0  = 5 : fp (11) (5)
                  | otherwise = . . .
            where
                (11,0) = divMod 55 (5)

    ** Se obtiene "2 : 2 : (5) : fp (11) (5)".


-- Paso #7:
            fp (11) 5 | (2) < (5) = [(11)]
                      | False     = . . .
                      | otherwise = . . .
            where
                (2,1) = divMod (11) 5

    ** Se obtiene "2 : 2 : 5 : [(11)]".


-- Paso #8:
    ** "factPrimos 220 = 2 : 2 : 5 : [11] = 2 : 2 : [5,11] = 2 : [2,5,11] = [2,2,5,11]"
-}


-- b)

    -- Creo que tiene algo quee ver con que en la función "divMod" el divisor                      *
    -- que resulta un valor válido siempre es primo, excepto en el último caso,
    -- donde lo es el cociente.

    -- Finaliza cuando "x' < d" porque no puede hacerse la división entera
    -- donde el divisor "d" es mayor que el dividendo "x'". Por lo que, no
    -- podría usarse la función "divMod" (en la función "fp") que realiza
    -- la división entera de dos números y devuelve una tupla con los
    -- valores cociente y resto de dicha división.


-- c)
factPrimos' :: Integer -> [Integer]
factPrimos' x = fp x 2
    where
        fp x d | x' < d    = [x]
               | r == 0    = d : fp x' d
               | otherwise = fp x (d')
            where
                d' = if even (d+1) && (d+1) /= 2 then (d+2) else (d+1)
                (x',r) = divMod x d        -- cociente y resto

    -- Idea (mía): El valor "d" es el que no debe ser par, por lo que
    -- detectando con un "if-then-else" si el valor "d" será par en su
    -- incremento (tercera guarda de "fp"), podría omitirse dicho valor
    -- "saltándose" el valor par e incrementando a un nuevo valor impar.

    -- Otra idea que tenía (más complicada de implementar) era que el valor
    -- "d" incrementase al siguiente primo directamente, lo cuál sería una
    -- implementación aún más eficiente que la anterior. Para ello, haría
    -- algo así como que "[ n | d <- [2..x], esPrimo d]", donde "n" sería el
    -- valor sobre el que se iría actualizando "d".


-- d)
p_factores n = True ==> (product $ factPrimos n) == n

    -- +++ OK, passed 100 tests.

--------------------------------------
-- Ejercicio 19
--------------------------------------
--------------------------------------
-- Ejercicio 20
--------------------------------------
--------------------------------------
-- Ejercicio 21
--------------------------------------
--------------------------------------
-- Ejercicio 22
--------------------------------------
--------------------------------------
-- Ejercicio 23
--------------------------------------