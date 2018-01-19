module DataStructures.Graph.GraphES
    ( Grafo
    , Arista
    , Camino

    , crearGrafoSucesores
    , crearGrafoAristas

    , sucesores
    , vertices
    , aristas
    , grado
    ) where

import Data.List

data Grafo a = G [a] (a -> [a])

type Camino a = [a]

vacio :: Grafo a
vacio = G [] (\_ -> [])

-- Crea un grafo vacío del tipo indicado.

-- «(\_ -> [])» es una función anónima que para cualquier
-- variable lambda «\_» esta siempre devuelve la lista «[]».


estaVacio :: Grafo a -> Bool
estaVacio (G vs _) = null vs

-- Devuelve lo que devuelva «null vs».

-- «vs» es la lista de vértices que componen el grafo.
-- * Un grafo está vacío si su lista «vs» es «[]».


crearGrafoSucesores :: (Eq a) => [a] -> (a -> [a]) -> Grafo a
crearGrafoSucesores vs suc = G (nub vs) aux
    where
        aux v = nub (suc v)

-- Genera un grafo a partir de sucesores de todos los vértices.

-- «vs» es la lista de todos los vértices del grafo.
-- «suc» es la función (local) que genera los vértices
   -- adyacentes («sucesores») a un vértice específico.


type Arista a = (a,a)

crearGrafoAristas :: (Eq a) => [a] -> [Arista a] -> Grafo a
crearGrafoAristas vs as = G vs suc
    where
        suc v = nub ([ y | (x,y) <- as, x == v ] ++ [ x | (x,y) <- as, y == v ])

-- Genera un grafo a partir de una lista de vértices y una de sus aristas.

-- «vs» es la lista de todos los vértices del grafo.
-- «as» es la lista de todas las aristas del grafo.
-- «suc» es la función que genera los vértices adyacentes
   -- («sucesores») a un vértice en concreto conocidas las
   -- aristas.


comparaAristas :: (Eq a) => Arista a -> Arista a -> Bool
comparaAristas a@(x,y) a' = a == a' || (y,x) == a'

-- Compara que la arista «a» sea igual que la arista «a'».

-- «x» e «y» son los extremos (vértices) de la arista.
-- * Dos aristas son iguales si sus componentes «x» e «y» lo son,
--   es decir, concectan los mismos vértices (sin importar el orden). 


aristas :: (Eq a) => Grafo a -> [Arista a]
aristas (G vs suc) = nubBy comparaAristas [ (v,w) | v <- vs, w <- suc v ]

-- aristas' (G vs suc) = comparaAristas [ (v,w) | v <- vs, w <- suc v]

-- Devuelve una lista con todas las aristas del grafo.

-- Para ello, inserta en «v» los vértices de la lista de todos
-- los vértices del grafo y en «w» los vértices sucesores de «v»;
-- de forma que consigue todas las aristas del grafo, repetidas,
-- pero son filtradas por «nubBy» para eliminar las excedentes.


sucesores :: (Eq a) => Grafo a -> a -> [a]
sucesores (G vs suc) v = aux v
    where
        aux v | v `elem` vs = suc v
              | otherwise   = []

-- Devuelve una lista con los vértices adyascentes («sucesores») a «v».

-- «v» es un vértice específico.


vertices :: Grafo a -> [a]
vertices (G vs suc) = vs

-- Devuelve la lista de los valores de los vértices
-- del grafo «(G vs suc)», que la por propia definición
-- del tipo de dato «Grafo», es el argumento «vs».


grado :: (Eq a) => Grafo a -> a -> Int
grado g v = length (sucesores g v)

-- Devuelve el grado del grafo.

-- El grado de un grafo corresponde con
-- la cantidad de vértices que tiene.
-- Como la función «sucesores» devuelve una lista con
-- todos los vértices, basta con saber la longitud de
-- dicha lista.


instance (Eq a, Show a) => Show (Grafo a) where
    show g@(G vs suc) = "Grafo{"++vertices++", "++arcos++"}"
        where
            vertices     = "[" ++ intercalate "," (map show vs) ++ "]"
            arcos        = "[" ++ intercalate "," (map showEd $ aristas g) ++ "]"
            showEd (x,y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- Muestra un grafo.

-- Un grafo será representado visualmente como
-- «Grafo{[vértices], [(arista1),(arista2),..,(aristaN)]}».
-- Es decir, primero su lista de vértices y después su
-- lista de aristas.
-- Se usa «arcos» para que el nombre no coincida con el
-- de la función «aristas», usada en la misma línea.