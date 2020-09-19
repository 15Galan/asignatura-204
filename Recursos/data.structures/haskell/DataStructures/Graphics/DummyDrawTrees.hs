-------------------------------------------------------------------------------
-- Mock functions to draw trees
-- Stripped-down version of DrawTrees
-- Pablo López, 2014
--
-------------------------------------------------------------------------------

module DataStructures.Graphics.DummyDrawTrees
  ( Subtrees(subtrees)
  , ShowNode(showNode)

  , VisualConf(..)
  , TreeConf
  , defaultVisualConf
  , drawTreeConf
  , outlineTreeConf

  , _drawOnWithConf
  , _drawOnWith
  , drawOn
  , outlineOn

  ) where

class Subtrees t where
  subtrees :: t -> [t]

class ShowNode t where
  showNode :: t -> String

data VisualConf = VC ()

defaultVisualConf = VC undefined

data TreeConf = TC ()

drawTreeConf = TC undefined

outlineTreeConf = TC undefined

_drawOnWithConf :: (Subtrees t) => FilePath -> TreeConf -> VisualConf -> (t -> String) -> t -> IO ()
_drawOnWithConf file tConf vConf showKV t = undefined

outlineOn :: (Subtrees t) => FilePath -> t -> IO ()
outlineOn file = undefined

_drawOnWith :: (Subtrees t) => FilePath -> (t -> String) -> t -> IO ()
_drawOnWith file = undefined

drawOn :: (Subtrees t, ShowNode t) => FilePath -> t -> IO ()
drawOn file = undefined
