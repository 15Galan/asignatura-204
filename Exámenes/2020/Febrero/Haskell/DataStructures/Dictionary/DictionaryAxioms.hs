-------------------------------------------------------------------------------
-- Axioms for Dictionaries
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2015
-------------------------------------------------------------------------------

module DataStructures.Set.DictionaryAxioms(ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9,ax10,ax11,ax12,ax13,dictionaryAxioms) where

import DataStructures.Dictionary.SortedLinearDictionary
--import DataStructures.Dictionary.BSTDictionary
--import DataStructures.Dictionary.AVLDictionary
import Test.QuickCheck
import qualified DataStructures.Util.TestDatatype as TDT

-- type Key = Int -- Test axioms using Ints as keys
type Key = TDT.Elem
type Value = Int


-- Axioms for basic set of operations
ax1       = True ==> isEmpty empty
ax2 k v d = True ==> not (isEmpty (insert k v d))

ax3 k          = True ==> not (isDefinedAt k empty )
ax4 k1 k2 v2 d = True ==> isDefinedAt k1 (insert k2 v2 d) == (k1==k2) || isDefinedAt k1 d

ax5 k          = True     ==> valueOf k (empty :: Dictionary Key Value) == Nothing
ax6 k1 k2 v2 d = (k1==k2) ==> valueOf k1 (insert k2 v2 d) == Just v2
ax7 k1 k2 v2 d = (k1/=k2) ==> valueOf k1 (insert k2 v2 d) == valueOf k1 d

ax8       = True                  ==> size (empty :: Dictionary Key Value) == 0
ax9 k v d = isDefinedAt k d       ==> size (insert k v d) == size d
ax10 k v d = not (isDefinedAt k d) ==> size (insert k v d) == 1 + size d

ax11 k           = True   ==> delete k (empty :: Dictionary Key Value) == empty
ax12 k1 k2 v2 d  = k1==k2 ==> delete k1 (insert k2 v2 d) == delete k1 d
ax13 k1 k2 v2 d = k1/=k2 ==> delete k1 (insert k2 v2 d) == insert k2 v2 (delete k1 d)


dictionaryAxioms = do
  quickCheck ( ax1 :: Property)
  quickCheck ( ax2 :: Key -> Value -> Dictionary Key Value -> Property)
  quickCheck ( ax3 :: Key -> Property)
  quickCheck ( ax4 :: Key -> Key -> Value -> Dictionary Key Value -> Property)
  quickCheck ( ax5 :: Key -> Property)
  quickCheck ( ax6 :: Key -> Key -> Value -> Dictionary Key Value -> Property)
  quickCheck ( ax7 :: Key -> Key -> Value -> Dictionary Key Value -> Property)
  quickCheck ( ax8 :: Property)
  quickCheck ( ax9 :: Key -> Value -> Dictionary Key Value -> Property)
  quickCheck (ax10 :: Key -> Value -> Dictionary Key Value -> Property)
  quickCheck (ax11 :: Key -> Property)
  quickCheck (ax12 :: Key -> Key -> Value -> Dictionary Key Value -> Property)
  quickCheck (ax13 :: Key -> Key -> Value -> Dictionary Key Value -> Property)
