module BiPartite(
    biColored    -- :: Ord v  => Graph v -> Maybe (D.Dictionary v Color)
  , Color(..)
  )  where

import Data.Maybe(isJust,fromJust)
import DataStructures.Graph.Graph
import qualified DataStructures.Stack.LinearStack as S
import qualified DataStructures.Dictionary.AVLDictionary as D


data Color = Red | Blue deriving (Eq,Show,Ord)

nextColor :: Color -> Color
nextColor Red  = Blue
nextColor Blue = Red


pushAll :: S.Stack a -> [a] -> S.Stack a
pushAll = foldr S.push   


biColored :: Ord v => Graph v -> Maybe (D.Dictionary v Color)
biColored g | null vs   = Just D.empty                                 -- empty graph is bipartite
            | otherwise = aux g D.empty (S.push (src ,Red) S.empty)
    where 
        vs  = vertices g
        src = head vs       -- initial vertex

 
aux :: Ord v => Graph v -> D.Dictionary v Color -> S.Stack (v, Color) -> Maybe (D.Dictionary v Color)
aux g dict stack | S.isEmpty stack = Just dict                                     -- (1)
                 | colored v = if (c == c') then aux g dict stack' else Nothing    -- then (d) else (c)
                 | otherwise = aux g dict' (pushAll stack' vc)                     -- (a)

{-    | Si la pila está vacía, devuelve el valor del diccionario (vértices coloreados).
      |
      | Si «v» fue coloreado (visitado), se comprueba que los colores coincidan.
      |     Si estos coinciden: el bucle debe seguir; si no, acaba (Nothing).
      |
      | Si «v» no fue coloreado (visitado), debe insertarse en el diccionario y además,
      | todos sus sucesores deben ser introducidos en la pila con el color opuesto con
      | el que «v» es introducido en el diccionario.
-}
    where
        colored v = D.isDefinedAt v dict
        (v,c)  = S.top stack
        stack' = S.pop stack
        dict'  = D.insert v c dict    -- (b.1)
        c'     = fromJust (D.valueOf v dict)
        vc     = [ (s, nextColor c) | s <- successors g v, not(colored s) ]    -- (b.2)

{-       colored v : «v» coloreado (visitado), está en el diccionario.

         (v,c)  : Valor (tupla) de la cima de la pila.
         stack' : Nueva pila, sin la cabeza.
         dict'  : Nuevo diccionario, con un elemento más.
         c'     : Color del vértice «v» en el diccionario.
         vc     : Lista de tuplas con los sucesores de «v» y el color que les correspondería.
-}


 
----------------
--- EXAMPLES ---
----------------
data MiVertice = A|B|C|D|E|F|G deriving (Show,Eq,Enum,Ord)

{- 
  A--B--D--F    
   \ |  |
     C--E--G
-}

g1  = mkGraphSuc vertices suc 
  where 
        vertices = [A .. G]

        suc A = [C,B]
        suc B = [A,C,D]
        suc C = [B,E]
        suc D = [B,F,E]
        suc E = [C,D,G]
        suc F = [D,D]
        suc G = [E]

-- *BiPartite> biColored g1
-- Nothing

{- 
  A--B--D--F    
     |  |
     C--E
-}

g2  = mkGraphEdges vertices edges 
  where 
        vertices =  [A .. F]
        edges = [(A,B),(B,C),(B,D),(D,E),(D,F),(C,E)]

-- *BiPartite> biColored g2
-- Just Dictionary(A->Red,B->Blue,C->Red,D->Red,E->Blue,F->Blue)

-- construcción de los bipartitos K n m


k n m = mkGraphEdges vertices edges
  where 
        vertices = [1 .. n + m]
        edges = [ (r,a) | r <- [1 .. n], a <- [n+1 .. n+m] ] 

-- *BiPartite> biColored (k 2 3)
-- Just Dictionary(1->Red,2->Red,3->Blue,4->Blue,5->Blue)
