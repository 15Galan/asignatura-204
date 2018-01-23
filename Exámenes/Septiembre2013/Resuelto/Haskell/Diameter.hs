-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Examen de setiembre (4-9-2013)
-- 
-- Diámetro de un grafo conexo
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
--
-- Alumno: APELLIDOS, NOMBRE
--
-------------------------------------------------------------------------------

module Diameter(
    diameter    -- :: Ord v  => Graph v -> Int
  )  where

import DataStructures.Graph.Graph
import DataStructures.Graph.GraphBFT(
     bftPaths    -- :: (Ord a) => Graph a -> a -> [Path a]
  )

---------------
--- APARTADO 1
---------------
-- La excentricidad de un vértice v se define como la distancia máxima
-- entre v y el resto de los vértices.

-- Si el grafo es conexo, la excentricidad coincide con la profundidad
-- del  árbol de  búsqueda  en anchura  (BFST: Breadth-First  Spanning
-- Tree); con raíz el vértice v.

-- Defina la función
eccentricity :: (Ord v) => Graph v -> v -> Int

eccentricity g v0 = undefined

---------------
--- APARTADO 2
---------------
-- El  diámetro de  un grafo  conexo se  define como  la excentricidad
-- máxima de sus vértices.

diameter :: Ord v => Graph v -> Int

diameter g = undefined


---------------------
--- EXAMPLES --------
---------------------

data MiVertice = A|B|C|D|E|F|G|H deriving (Show,Eq,Enum,Ord)

{-
  A--B--C
   \ |\ |
     D  E--F--G
-}
g1  = mkGraphSuc vertices suc
  where
        vertices = [A .. G]
        suc A = [B,D]
        suc B = [A,C,D,E]
        suc C = [B,E]
	suc D = [A,B]
        suc E = [B,C,F]
        suc F = [E,G]
	suc G = [F]
{-
  A--B--D--F
     |  |
     C--E
-}
g2  = mkGraphEdges vertices edges
  where
        vertices =  [A .. F]
        edges = [(A,B),(B,C),(B,D),(C,E),(D,E),(D,F)]

{-
A--B--D--F
   |  |
   C  E--G
-}
g3  = mkGraphEdges vertices edges
  where
        vertices =  [A .. G]
        edges = [(A,B),(B,C),(B,D),(D,E),(D,F),(E,G)]

{-
1--2--3--4--5--6--7--8
               |     |
               10----9
-}
g4  = mkGraphEdges vertices edges
  where
        vertices =  [1 .. 9]
        edges = [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9)
                 ,(6,10),(9,10)  ]
