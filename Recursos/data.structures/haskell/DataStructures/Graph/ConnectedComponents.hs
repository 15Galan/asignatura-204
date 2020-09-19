-------------------------------------------------------------------------------
-- Connected Components of a Graph
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.ConnectedComponents
  ( dftConnectedComps
  ) where

import DataStructures.Graph.Graph
import DataStructures.Graph.GraphDFT
import Data.List((\\))

type ConnectedComponent a  = [a]

dftConnectedComps:: (Ord a) => Graph a -> [ ConnectedComponent a ]
dftConnectedComps g  = aux (vertices g)
 where
  aux []      = []
  aux (v:vs)  = comp : aux (vs \\ comp)
    where
      comp  = dft g v

