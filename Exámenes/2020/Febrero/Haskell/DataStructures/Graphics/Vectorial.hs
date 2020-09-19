-------------------------------------------------------------------------------
-- Data Structures. Grado en Informática. UMA.
-- @ Pepe Gallardo, 2015
--
-- Vectorial graphics
--
-------------------------------------------------------------------------------

module DataStructures.Graphics.Vectorial
  ( Percent

  , Color
  , rgb
  , blue
  , red
  , green
  , yellow
  , magenta
  , cyan
  , black
  , white
  , gray
  , orange

  , Point(..)
  , Radius
  , Modifier(..)
  , Transformation(..)
  , Graphics(..)
  , render
  , renderExt

  , nil
  , sec

  , point
  , line
  , arc
  , ellipse
  , circle
  , polygon
  , text
  ) where

import Text.PrettyPrint hiding (text, render, (<>))
import qualified Text.PrettyPrint as TP

type Percent = Double -- in [0.0 , 1.0]

data Color = C !Percent !Percent !Percent deriving (Eq,Show)

data Point = P !Double !Double deriving (Eq,Show)

type Radius = Double

type Radian = Double

data Modifier = Brush !Color
              | Fill !Color
              | Transparency !Percent
              | Thickness !Double
              | Dotted ![Double]
              | Font !String !Double
              | Antialias Bool
              | TextCentered Bool
              deriving Show

data Transformation = Translate !Double !Double
                    | Rotate !Radian
                    | Scale !Double !Double
                    deriving Show

infixr 9 :>

data Graphics = Nil
              | Point
              | Line !Point -- from origin to point
              | Arc !Radius !Radius !Radian !Radian
              | Polygon ![Point]
              | Text !String
              | With ![Modifier] !Graphics
              | Transformed ![Transformation] !Graphics
              | Graphics :> Graphics
              | Clip !Graphics !Graphics
              deriving Show

sec :: [Graphics] -> Graphics
sec = foldr (:>) Nil

nil :: Graphics
nil = Nil

point :: Point -> Graphics
point (P x y) = Transformed [ Translate x y ] Point

line :: Point -> Point -> Graphics
line (P x y) (P x' y') = Transformed [ Translate x y ] (Line (P (x'-x) (y'-y)))

arc :: Point -> Radius -> Radius -> Radian -> Radian -> Graphics
arc (P x y) rx ry alfa beta = Transformed [ Translate x y ] (Arc rx ry alfa beta)

ellipse :: Point -> Radius -> Radius -> Graphics
ellipse (P x y) rx ry = Transformed [ Translate x y ] (Arc rx ry 0 (2*pi-0.001))

circle :: Point -> Radius -> Graphics
circle p r = ellipse p r r

polygon :: [Point] -> Graphics
polygon = Polygon

text :: Point -> String -> Graphics
text (P x y) str = Transformed [ Translate x y ] (Text str)

rgb :: Percent -> Percent -> Percent -> Color
rgb r g b = C r g b

blue     = rgb 0 0 1
red      = rgb 1 0 0
green    = rgb 0 0.7 0
yellow   = rgb 1 1 0
magenta  = rgb 1 0 1
cyan     = rgb 0 1 1
black    = rgb 0 0 0
white    = rgb 1 1 1
gray     = rgb 0.5 0.5 0.5
orange   = rgb 1 0.37 0


type Elem = String
type Attr = String
type Val = Doc

data Xml = XSimple Elem [(Attr, Val)]
         | XNested Elem [(Attr, Val)] Xml
         | XSeq [Xml]
         | XText String

(><) :: Doc -> Doc -> Doc
d >< d' = d TP.<> d'
         
txt :: String -> Doc
txt str = TP.text str

pp :: Xml -> Doc
pp (XSimple el ps)     =
  hcat [ txt "<", txt el, ifNotNull ps space, pairs ps, txt "/>" ]
pp (XNested el ps xml) =
  if el=="text" then e1 >< e2 >< e3 -- nesting in text introduces unwanted spaces
                else e1 $$ nest 1 e2 $$ e3
  where
   e1 = hcat [ txt "<", txt el, ifNotNull ps space, pairs ps, txt ">" ]
   e2 = pp xml
   e3 = hcat[ txt "</", txt el, txt ">" ]
pp (XSeq xs)           =
  vcat (map pp xs)
pp (XText xs)          =
  txt xs

ifNotNull xs ys = if null xs then empty else ys

pairs ps = hcat . punctuate space . map pair $ ps
 where pair (attr, val) = hcat [ txt attr, equals, doubleQuotes val ]

modVal :: Modifier -> [(Attr, Doc)]
modVal (Brush c)        = [("stroke", _rgb c)]
modVal (Fill c)         = [("fill", _rgb c)]
modVal (Thickness w)    = [("stroke-width", double w)]
modVal (Transparency n) = [("opacity", double n)]
modVal (TextCentered b) = [("text-anchor", txt $ if b then "middle" else "start")]
modVal (Font xs h)      = [("font-family", txt xs), ("font-size", double h >< txt "px")]
modVal (Antialias b)    = [("shape-rendering", txt $ if b then "geometricPrecision" else "optimizeSpeed")]
modVal (Dotted xs)      = [("stroke-dasharray", hcat (punctuate comma (map double xs)) >< semi)]

modVals = hcat . punctuate semi . concatMap (map ((\(x,y)->txt x >< colon >< y)) . modVal)

transfVal :: Transformation -> [(Attr, Doc)]
transfVal (Translate x y) = [("translate", parens (hcat [double x, comma, double y]))]
transfVal (Scale x y)     = [("scale", parens (hcat [double x, comma, double y]))]
transfVal (Rotate x)      = [("rotate", parens (double (toDeg x)))]
 where toDeg alpha = alpha*180/pi

transfVals = hcat . punctuate space . concatMap (map ((\(x,y)->txt x><y)) . transfVal)


_rgb (C r g b) = txt "rgb" >< parens (hcat (punctuate comma (map num [r,g,b])))
  where num x = integer (truncate (x*255))

_simple tag xs = XSimple tag xs

_nested tag xs ys = XNested tag xs ys

_g :: [(Attr, Val)] -> Xml -> Xml
_g = _nested "g"

_polygon ps = _simple "polygon" [("points", coords)]
 where
  coords = hcat . punctuate space . map coord $ ps
  coord (P x y) = double x >< comma >< double y

_point = _simple "circle" [("cx", integer 0),("cy", integer 0),("r", integer 1)]

_line (P x y) = _simple "line" [("x1", integer 0),("y1", integer 0),("x2", double x),("y2", double y)]

_text xs = _g [("transform", txt "scale(1 -1)")] (_nested "text" [] (XText xs))

_arc rx ry alpha0 beta0 = _simple "path" [("d", args)]
 where
  (alpha,beta) = if alpha0<beta0 then (alpha0,beta0) else (beta0,alpha0)
  params ang = (rx*cos ang, ry*sin ang)
  (x0,y0) = params beta
  (x1,y1) = params alpha
  args = hcat (punctuate space [txt "M", double x0, double y0, txt "A", double rx, double ry, integer 0, arcSweep, integer 0, double x1, double y1])
  arcSweep = integer $ if beta - alpha <= pi then 0 else 1


toXml :: Graphics -> Xml
toXml Nil                     = XSeq []
toXml Point                   = _point
toXml (Line p)                = _line p
toXml (Polygon ps)            = _polygon ps
toXml (Text xs)               = _text xs
toXml (Arc rx ry alpha beta)  = _arc rx ry alpha beta
toXml (With ms g)             = _g [("style", modVals ms)] (toXml g)
toXml (Transformed ts g)      = _g [("transform", transfVals ts)] (toXml g)
toXml (g1 :> g2)              = XSeq [toXml g1, toXml g2]



render = renderExt 800 600 800 600 "DataStructures.Vectorial.svg"

renderExt pixelX pixelY escX escY file g = do
    writeFile file (begin++xml++end)
    putStrLn $ "Output generated on file \""++file++"\""
     where
      pixelXD = fromIntegral pixelX :: Double
      pixelYD = fromIntegral pixelY :: Double
      pixelScX = pixelXD/escX
      pixelScY = pixelYD/escY
      back = With [Fill white] $ Polygon [P 0 0, P pixelXD 0, P pixelXD pixelYD, P 0 pixelYD ]
      begin = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n<svg xmlns=\"http://www.w3.org/2000/svg\" height=\""++show pixelY++"\" width=\""++show pixelX++"\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.11.1\" style=\"shape-rendering:geometricPrecision; text-rendering:geometricPrecision; image-rendering:optimizeQuality; fill-rule:evenodd; clip-rule:evenodd\">\n"
      end = "\n</svg>"
      xml = TP.render . pp . toXml $
              sec [ back
                  , With [Brush black, Fill white] $
                    Transformed [ Translate (pixelXD/2) (pixelYD/2)
                                , Scale pixelScX (-pixelScY)
                                ] g
                  ]
