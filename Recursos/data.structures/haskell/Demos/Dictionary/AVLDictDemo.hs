-------------------------------------------------------------------------------
-- Demo for dictionaries implemented by using AVL Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2015
-------------------------------------------------------------------------------

module Demos.Dictionary.AVLDictDemo where

import DataStructures.Dictionary.AVLDictionary
import qualified DataStructures.Set.SortedLinearSet as S
import DataStructures.Set.SortedLinearSet(Set)

d1 :: Dictionary String Int
d1 = insert "Malaga" 952 (insert "Sevilla" 954 (insert "Cadiz" 956 empty))

d2 :: Dictionary String (Set Int)
d2 = insert "Malaga" s1 (insert "Sevilla" s2 (insert "Cadiz" s3 empty))
 where
        mkSet = foldr S.insert S.empty
        s1 = mkSet [951,952,851,852]
        s2 = mkSet [954,955,854,855]
        s3 = mkSet [956,856]
