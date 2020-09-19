module DisjointSetDemo where

import DataStructures.Set.DisjointSet

{- |

>>> numbers
DictionaryDisjointSet((0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9),(10,10),(11,11),(12,12),(13,13),(14,14),(15,15))

-}

numbers :: DisjointSet Integer
numbers = foldr add empty [0..15]

{- |

>>> mod3
DictionaryDisjointSet((0,0),(1,1),(2,2),(3,0),(4,1),(5,2),(6,3),(7,4),(8,2),(9,3),(10,4),(11,2),(12,9),(13,10),(14,5),(15,3))

-}

mod3 :: DisjointSet Integer
mod3 = foldr (\ (x,y) ds -> union x y ds) numbers [ (0,3), (6,9), (12,15)
                                                  , (1,4), (7,10)
                                                  , (2,5), (8,11)
                                                  , (15,3)
                                                  , (4,13)
                                                  , (14,5)
                                                  , (9,12)
                                                  , (10,13)
                                                  , (2,11)
                                                  ]
