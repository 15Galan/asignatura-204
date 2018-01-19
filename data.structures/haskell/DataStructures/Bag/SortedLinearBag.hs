-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 3 - Implementación y Especificación del TAD Bag
--
-- Alumno: GALÁN HERRRERA, ANTONIO
--
-------------------------------------------------------------------------------

module DataStructures.Bag.SortedLinearBag
  ( Bag         -- :: Bag a
  , empty       -- :: Bag a
  , isEmpty     -- :: Bag a -> Bool
  , insert      -- :: Ord a => a -> Bag a -> Bag a
  , delete      -- :: Ord a => a -> Bag a -> Bag a
  , occurrences -- :: Ord a => a -> Bag a -> Int
  , foldBag     -- :: Ord a => (a -> Int -> b -> b) -> b -> Bag a -> b
  ) where

import Test.QuickCheck
import Data.List(sort, (\\))

-------------------------------------------------------------------------------
-- Implementación del TAD Bag
-------------------------------------------------------------------------------

data Bag a = Empty
           | Node a Int (Bag a)
           deriving Eq

-- mantenemos los objetos de los nodos ordenados
bolsa1 :: Bag Char
bolsa1 = Node 'a' 2 (Node 'b' 3 (Node 'c' 2 (Node 'd'  1 Empty)))

{-

Consideraremos el siguiente INVARIANTE de la representación:

   I1 : la información de los nodos estará ordenada sin duplicidades:
                   Nodo x ox (Nodo y oy s) ==>> x < y

   I2 : no deben aparecer nodos con el contador de ocurrencias igual <= 0
                     Nodo x ox s   ==>> ox > 0

Esto permitirá generar una igualdad estructural (deriving Eq).

Todas las operaciones del TAD Bag reciben una bolsa que satisface el
invariante y, si devuelven una bolsa, ésta debe satisfacer el invariante.

-}

-- convertir una lista en una bolsa
list2Bag:: Ord a => [a] -> Bag a
list2Bag = foldr insert empty

{-

La función list2Bag permite convertir una lista en una bolsa.
Por ejemplo:

   list2Bag "abracadabra"

devuelve la bolsa:

   Node 'a' 5 (Node 'b' 2 (Node 'c' 1 (Node 'd' 1 (Node 'r' 2 Empty))))

que se representa mediante show como:

   LinearBag { 'a' 'a' 'a' 'a' 'a' 'b' 'b' 'c' 'd' 'r' 'r' }

Puedes usar list2Bag para construir bolsas para comprobar las
funciones de bolsas:

   *LinearBag> delete 'a' (list2Bag "haskell")
   LinearBag { 'e' 'h' 'k' 'l' 'l' 's' }

   *LinearBag> delete 'l' (list2Bag "haskell")
   LinearBag { 'a' 'e' 'h' 'k' 'l' 's' }

   *LinearBag> delete 'x' (list2Bag "haskell")
   LinearBag { 'a' 'e' 'h' 'k' 'l' 'l' 's' }

   *LinearBag> delete 'a' (list2Bag "haskell") == list2Bag "hskell"
   True

-}

--------------------------------------------------------
-- EJERCICIO 1. COMPLETAR LA ESPECIFICACION
--------------------------------------------------------

-- devuelve una bolsa vacía
empty :: Bag a
empty = Empty


-- comprueba si una bolsa está vacía
isEmpty :: Bag a -> Bool
isEmpty Empty = True
isEmpty _     = False


-- inserta un nuevo dato en una bolsa
insert :: Ord a => a -> Bag a -> Bag a
insert x Empty = Node x 1 Empty
insert x (Node y oy s) | x < y     = Node x 1 (Node y n b)
                       | x == y    = Node y (n+1) b
                       | otherwise = Node y n (insert x b)
                       

-- devuelve el número de ocurrencias de un elemento en una bolsa
-- (0 si el elemento no está)
occurrences :: (Ord a) => a -> Bag a -> Int
occurrences x Empty = 0
occurrences x (Node y oy s) | x < y     = 0
                            | x == y    = n
                            | otherwise = occurrences x b


-- borra una ocurrencia de un dato de una bolsa
-- (devuelve la bolsa original si el dato no estaba en la bolsa)
delete :: (Ord a) => a -> Bag a -> Bag a
delete x Empty = Empty
delete x (Node y oy s) | x < y     = Node y n b
                       | x == y    = if (n > 1) then Node y (n-1) b else b
                       | otherwise = delete x b

--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------

-- instancia de la clase Show para imprimir las bolsas
instance (Show a) => Show (Bag a) where
   show s = "LinearBag { " ++ show' s
    where
       show' Empty = "}"
       show' (Node x ox s) = muestra x ox ++  show' s
       muestra x 0  = ""
       muestra x ox = show x ++ ' ' : muestra x (ox-1)

-------------------------------------------------------------------------------
-- Especificación del TAD Bag
-------------------------------------------------------------------------------

-- generación de bolsas aleatorias para QuickCheck
instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
  arbitrary =  do
                  xs <- listOf arbitrary
                  return (foldr insert empty xs)

--------------------------------------------------------
-- EJERCICIO 2. COMPLETAR LOS AXIOMAS
--------------------------------------------------------

-- selectores
isEmpty_empty = isEmpty empty == True
isEmpty_insert x s = isEmpty (insert x s) == False

occurrences_empty x = occurrences x empty == 0
occurrences_insert_1 x s = occurrences x (insert x s) == 1
occurrences_insert_2 x y s = x /= y ==> occurrences x (insert y s) == 0

-- transformadores
delete_empty x = delete x empty == Empty
delete_insert_1 x s = delete x (insert x s) == Empty
delete_insert_2 x y s  = x /= y ==> delete x (insert y s) == (Node y s)

--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------

type T = Char -- Integer, etc.

checkBag = do
               quickCheck (isEmpty_empty :: Bool)
               quickCheck (isEmpty_insert :: T -> Bag T -> Bool)
               quickCheck (occurrences_empty :: T -> Bool)
               quickCheck (occurrences_insert_1 :: T -> Bag T -> Bool)
               quickCheck (occurrences_insert_2 :: T -> T -> Bag T -> Property)
               quickCheck (delete_empty :: T -> Bool)
               quickCheck (delete_insert_1 :: T -> Bag T -> Bool)
               quickCheck (delete_insert_2 :: T -> T -> Bag T -> Property)

-------------------------------------------------------------------------------
-- Operaciones auxiliares del TAD Bag
-------------------------------------------------------------------------------

{-

Añadir al módulo las siguientes funciones para bolsas:

   unión de bolsas
   intersección de bolsas
   diferencia de bolsas

Estas funciones son semejantes a las de los conjuntos pero teniendo
en cuenta las ocurrencias de cada elemento.

-}

--------------------------------------------------------
-- EJERCICIO 3. COMPLETAR FUNCIONES AUXILIARES
--------------------------------------------------------

union :: Ord a => Bag a -> Bag a -> Bag a
union s Empty = s
union Empty s = s
union (Node x ox s) (Node y oy t) | x < y     = Node x ox (union s (Node y oy t))
                                  | x == y    = Node x (ox+oy) (union s t)
                                  | otherwise = Node y oy (union (Node x ox s) t)

intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection s Empty = s
intersection Empty s = s
intersection (Node x ox s) (Node y oy t) | x < y     = intersection s (Node y oy t)
                                         | x == y    = Node x (min ox oy) (intersection s t)
                                         | otherwise = intersection (Node x ox s) t

difference :: Ord a => Bag a -> Bag a -> Bag a
difference s Empty = s
difference Empty s = Empty
difference (Node x ox s) (Node y oy t) | x < y             = Node x ox (difference s (Node y oy t))
                                       | x == y && ox > oy = Node x (ox-oy) (difference s t)
                                       | x == y            = difference s t
                                       | otherwise         = difference (Node x ox s) t

--------------------------------------------------------
--------------------------------------------------------
--------------------------------------------------------

-- propiedades QuickCheck para comprobar union, intersection y difference

check_union xs ys =
   union (list2Bag xs) (list2Bag ys) == list2Bag (xs ++ ys)

check_intersection xs ys =
   intersection (list2Bag xs) (list2Bag ys) == list2Bag (intersecta (sort xs) (sort ys))
    where
      intersecta [] _ = []
      intersecta _ [] = []
      intersecta (x:xs) (y:ys)
        | x == y = x : intersecta xs ys
        | x <  y = intersecta xs (y:ys)
        | otherwise = intersecta (x:xs) ys

check_difference xs ys =
   difference (list2Bag xs) (list2Bag ys) == list2Bag (xs \\ ys)

checkBagAux = do
                   quickCheck (check_union :: [T] -> [T] -> Bool)
                   quickCheck (check_intersection :: [T] -> [T] -> Bool)
                   quickCheck (check_difference :: [T] -> [T] -> Bool)


--------------------------------------------------------
-- EJERCICIO 4. COMPLETAR COMPLEJIDAD
--------------------------------------------------------

-------------------------------------------------------------------------------
-- Eficiencia de la implementación del TAD Bag
-------------------------------------------------------------------------------

{-

Responde a las dos siguientes preguntas sobre la eficiencia de nuestra
representación del TAD Bag.

1) Completa la siguiente tabla indicando mediante la notación O el número
de pasos que realiza cada operación del TAD Bag

    operación        números de pasos
    ---------------------------------
    empty                O(1)
    isEmpty              O(1)
    insert               O(n)
    delete               O(n)
    occurrences          O(n)



2) Nuestra representación del TAD Bag mantiene la secuencia de nodos ordenados:

        Node 'a' 2 (Node 'b' 3 (Node 'c' 2 (Node 'd'  1 Empty)))

Obviamente, también es posible representar el TAD Bag por una secuencia desordenada:

        Node 'c' 2 (Node 'd' 1 (Node 'a' 2 (Node 'b'  3 Empty)))

¿Cuál de las dos representaciones es más eficiente? ¿Por qué?

    La primera, porque ya está ordenada.

-}

-------------------------------------------------------------------------------
-- Plegado del TAD Bag
-------------------------------------------------------------------------------

-- función de plegado para bolsas

foldBag :: Ord a => (a -> Int -> b -> b) -> b -> Bag a -> b
foldBag f z Empty = z
foldBag f z (Node x ox s) = f x ox (foldBag f z s)

-- ejemplo de uso del plegado

-- dada una bolsa 'bag' devuelve una lista con las
-- claves que aparecen en la bolsa

keys :: Ord a => Bag a -> [a]
keys bag = foldBag (\e oe s -> e:s) [] bag
