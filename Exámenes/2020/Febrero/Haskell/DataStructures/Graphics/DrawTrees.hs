-------------------------------------------------------------------------------
-- Functions to draw trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graphics.DrawTrees
  ( Subtrees(subtrees, isEmptyTree)
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


import DataStructures.Graphics.Vectorial

data Render = Render Graphics Double Double Double Double
  --  Graphics
  --  width of tree
  --  height of tree
  --  absolute position for horizontal center of node
  --  absolute position for vertical center of node


graphics (Render g _ _ _ _) = g

width (Render _ w _ _ _) = w

height (Render _ _ h _ _) = h

centerX (Render _ _ _ cx _) = cx

centerY (Render _ _ _ _ cy) = cy


class Subtrees t where
  subtrees :: t -> [t]
  isEmptyTree :: t -> Bool

class ShowNode t where
  showNode :: t -> String



data VisualConf =
  VC { shadowC, brushC, fillC, lineC, textC :: Color
     , fontName :: String
     , fontSz :: Double
     }

defaultVisualConf2 =
  VC { shadowC = rgb 0.6 0.6 0.6
     , brushC = rgb 0.65 0.28 0.02-- 0 0.1 0.7
     , fillC = rgb 1 0.75 0 -- 0.99 0.99 0.7
     , lineC = rgb 0 0 0
     , textC = rgb 0 0 0
     , fontName = "Arial"
     , fontSz = 17.5
     }

defaultVisualConf =
  VC { shadowC = rgb 0.6 0.6 0.6
     , brushC = rgb 0.40 0.70 0.41-- 0 0.1 0.7
     , fillC = rgb 0.69 0.96 0.69 -- 0.99 0.99 0.7
     , lineC = rgb 0 0.44 0.75
     , textC = rgb 0.8 0 0
     , fontName = "Arial"
     , fontSz = 17.5
     }

data TreeConf =
  TC { rEmpty, wEmpty, hEmpty
     , rNode, hNode :: Double
     }


drawTreeConf =
  TC { rEmpty = r0
     , wEmpty = 2*r0+12
     , hEmpty = 2*r1
     , rNode = r1
     , hNode = 2*r1+10
     }
 where
  r0 = 3
  r1 = 14

outlineTreeConf =
  TC { rEmpty = r0
     , wEmpty = 10
     , hEmpty = 2*r1
     , rNode = r1
     , hNode = 2*r1+8
     }
 where
  r0 = 2.5
  r1 = 7


_drawOnWithConf :: (Subtrees t) => FilePath -> TreeConf -> VisualConf -> (t -> String) -> t -> IO ()
_drawOnWithConf file tConf vConf showKV t =
  renderExt w' h' w h file $
    Transformed [Translate (-w0/2) (h0/2)] g
  -- putStrLn $ "Output generated on file \""++file++"\""
 where
   Render g w0 h0 _ _ = renderTree t tConf vConf showKV 0 0
   w = w0+35
   h = h0+10
   w' = round w
   h' = round h


outlineOn :: (Subtrees t) => FilePath -> t -> IO ()
outlineOn file = _drawOnWithConf file outlineTreeConf defaultVisualConf (const "")

_drawOnWith :: (Subtrees t) => FilePath -> (t -> String) -> t -> IO ()
_drawOnWith file = _drawOnWithConf file drawTreeConf defaultVisualConf

drawOn :: (Subtrees t, ShowNode t) => FilePath -> t -> IO ()
drawOn file = _drawOnWith file showNode


{-
drawKeysOn :: (DecomposableTree t) => FilePath -> t -> IO ()
drawKeysOn file = drawOnWith file (\k v -> show k)
-}

rect :: Point -> Double -> Double -> Graphics
rect (P x y) w h = polygon [ P (x-rx) (y-ry), P (x-rx) (y+ry)
                           , P (x+rx) (y+ry), P (x+rx) (y-ry)
                           ]
 where
   ry = h/2
   rx = w/2

oval :: Point -> Double -> Double -> Graphics
oval (P x y) w h =
  sec [ With [Thickness 0] $ sec [ rect (P x y) (wr+0.5) (h+0.5)
                                 , lArc
                                 , rArc
                                 ]
      , lArc
      , rArc
      , line (P lX tY) (P rX tY)
      , line (P lX bY) (P rX bY)
      ]
  where
   wr = w-2*ry
   ry = h/2
   lX = x-wr/2
   rX = x+wr/2
   tY = y+ry
   bY = y-ry
   lArc = arc (P lX y) ry ry (pi/2) (3*pi/2)
   rArc = arc (P rX y) ry ry (-pi/2) (pi/2)


renderTree :: (Subtrees t) => t -> TreeConf -> VisualConf -> (t -> String) -> Double -> Double -> Render
renderTree t
    | isEmptyTree t = renderEmpty
    | otherwise     = renderNode t subtrees


renderEmpty tConf vConf showKV dx dy =
--  Render (With [Brush (brushC vConf), Fill (fillC vConf),Thickness 1.5] $ nil {-rect (P cx cy) w w-})
  Render ( sec [ With [Brush (shadowC vConf), Fill (shadowC vConf),Thickness 1.5] $ rect (P (cx+1) (cy-1)) w w
               , With [Brush (brushC vConf), Fill (fillC vConf),Thickness 1.5] $ rect (P cx cy) w w
               ] )
           wE hE cx cy
   where
    rE = rEmpty tConf
    w = 2*rE
    wE = wEmpty tConf
    hE = hEmpty tConf
    cx = dx+wE/2
    cy = dy-rE

renderNode node getSubtrees tConf vConf showKV dx dy =
  Render g w h cx cy
   where
    hN = hNode tConf
    rN = rNode tConf
    ts = getSubtrees node



    rs = snd $ foldl (\(dx,rs) t -> let r = renderTree t tConf vConf showKV dx (dy-hN) in (dx+width r,r:rs))
                     (dx',[]) ts
    rs' = snd $ foldl (\(dx,rs) t -> let r = renderTree t tConf vConf showKV dx (dy-hN) in (dx+width r,r:rs))
                     (dx',[]) (filter (not.isEmptyTree) ts)

    centers = map centerX rs

    cx
     | null centers         = dx
     | odd (length centers) = centers !! (length centers `div` 2)
     | otherwise            = (head centers + last centers)/2
    cy = dy-rN
    wChilds = sum (map width rs)
    l = length ts
    oW = 2*fromIntegral (if l > 1 then l-1 else 1)*rN
    oH = 2*rN
    w = max wChilds (oW+6)
    h = hN + if null rs then 0 else maximum (map height rs)
    dx' = dx+(w-wChilds)/2
    ov x y = if length ts<=2 then circle (P (x+cx) (y+cy)) rN else oval (P (x+cx) (y+cy)) oW oH

    g = sec [ With [Brush (shadowC vConf), Fill (shadowC vConf)] $ ov 2 (-2)
            , With [Brush (lineC vConf), Thickness 2] $ sec [ line (P cx cy) (P (centerX r) (centerY r)) | r <- rs]
            , With [Brush (brushC vConf), Fill (fillC vConf), Thickness 2.5] $ ov 0 0
            , With [Brush (textC vConf), Fill (textC vConf), TextCentered True, Font (fontName vConf) (fontSz vConf) ] $ text (P cx (cy-6)) (showKV node)
--            , rect (P (dx+w/2) (dy-rN)) (w) h
            , sec (map graphics rs)
            ]
