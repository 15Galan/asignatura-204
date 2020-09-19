-------------------------------------------------------------------------------
-- Data Structures. Grado en Informática. UMA.
-- @ Pepe Gallardo
--
-- Vectorial graphics
--
-------------------------------------------------------------------------------

module DataStructures.Graphics.VectorialCairo 
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
  , pixels
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

import Data.Char
import Control.Monad
import Graphics.Rendering.Cairo hiding (Antialias,arc)
import qualified Graphics.Rendering.Cairo

import Graphics.Rendering.Cairo.Matrix(Matrix(..))
import System.IO.Unsafe
import Data.IORef



type Percent = Double -- entre 0.0 y 1.0

data Color = C !Percent !Percent !Percent deriving (Eq,Show)

data Point = P !Double !Double deriving (Eq,Show)

type Radius = Double

type Radian = Double

data Switch = Off | On deriving (Show,Eq,Ord)

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
              | Line !Point -- desde el origen al point
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
ellipse (P x y) rx ry = Transformed [ Translate x y ] (Arc rx ry 0 (2*pi))

circle :: Point -> Radius -> Graphics 
circle p r = ellipse p r r

polygon :: [Point] -> Graphics
polygon = Polygon

text :: Point -> String -> Graphics
text (P x y) str = Transformed [ Translate x y ] (Text str)


rgb :: Percent -> Percent -> Percent -> Color
rgb r g b = C r g b


blue = rgb 0 0 1
red = rgb 1 0 0
green = rgb 0 0.7 0
yellow = rgb 1 1 0
magenta = rgb 1 0 1
cyan = rgb 0 1 1
black= rgb 0 0 0
white = rgb 1 1 1
gray = rgb 0.5 0.5 0.5
orange = rgb 1 0.37 0

data Estado = 
  Est { _pincel :: Color
      , _relleno :: Maybe Color
      , _transparencia :: Double
      , _grosor :: Double
      , _discontinua :: [Double]
      , _fuente :: (String,Double)
      , _scX :: !Double
      , _scY :: !Double
      , _rot :: !Double
      , _pinta :: Render ()
      , _rellena :: Render ()
      , _antialias :: Graphics.Rendering.Cairo.Antialias
      , _textCentered :: Bool
      }

estado scX scY = 
         Est { _pincel = black
             , _transparencia = 0
             , _relleno = Nothing
             , _grosor = 1
             , _discontinua = []
             , _fuente = ("Arial",10)
             , _scX = scX
             , _scY = scY
             , _rot = 0
             , _pinta = stroke
             , _rellena = fill
             , _antialias = AntialiasDefault
             , _textCentered = False
             }

extensión :: FilePath -> String
extensión fich = map Data.Char.toLower . reverse . takeWhile (/='.') . reverse $ fich

--render = renderExt 400 400 200 200 "StdDraw.png"
render = renderExt 800 600 800 600 "DataStructures.Vectorial.png"

{-# NOINLINE pixelSc #-}
pixelSc :: IORef (Double,Double)
pixelSc = unsafePerformIO $ newIORef (1,1)

pixels n = unsafePerformIO $ do
 (scX,scY) <- readIORef pixelSc 
 return $ - n -- min scX scY * n
  
renderExt pixelX pixelY escX escY fich gr = do
    let pixelXD = fromIntegral pixelX :: Double
    let pixelYD = fromIntegral pixelY :: Double
    let pixelScX = escX/pixelXD
    let pixelScY = escY/pixelYD
    writeIORef pixelSc (pixelScX,pixelScY)
    extToWithSfc (extensión fich) fich pixelXD pixelYD $ \sfc ->
     renderWith sfc (do { setOperator {-OperatorSource-} OperatorOver
                        ; setMatrix $ Matrix (1*pixelXD/escX) 0 0 (-1*pixelYD/escY) (pixelXD/2) (pixelYD/2) 
                        ; setLineCap LineCapRound
                      --  ; setAntialias AntialiasNone
                       --; rotate (pi/2) 
                       --; translate (widthD/2) (-heightD/2)
                       -- ; scale (widthD/2) (heightD/2)
                        ; runM (estado (1/pixelScX) (1/pixelScY))  $ pro gr
                        })
    --surfaceWriteToPNG sfc fich

extToWithSfc ext = 
  let assoc = [("png",withPNGSurface), ("pdf",withPDFSurface), ("ps",withPSSurface), ("svg",withSVGSurface)]
  in case lookup ext assoc of
    Nothing -> error $ "Extensión "++ext++" desconocida"
    Just f  -> f 

withPNGSurface fich ancho alto f = 
  withImageSurface FormatARGB32 (round ancho) (round alto) $ \sfc -> do
    f sfc
    surfaceWriteToPNG sfc fich





data Par a = Par !Estado !a


data M a = M (Estado -> Render (Par a)) 

instance Monad M where
 return x = M $ \est -> return (Par est x)
 M f >>= g = M $ \est -> do (Par est' x) <- f est
                            let M f' = g x
                            f' est' 

runM :: Estado -> M () -> Render ()
runM est (M ac) = ac est >>= (\(Par _ x) -> return x) 

leerM :: M Estado
leerM = M $ \est -> return (Par est est)

escribirM :: Estado -> M ()
escribirM est = conM $ const est

conM :: (Estado -> Estado) -> M ()
conM f = M $ \est -> return (Par (f est) ())

renderM :: Render a -> M a
renderM m = M $ \est -> do x <- m; return (Par est x)

preservandoEstadoM :: M a -> M a
preservandoEstadoM ac = do
  est <- leerM
  x <- ac
  escribirM est
  return x

preservandoMatriz ac = do
  m <- getMatrix
  x <- ac
  setMatrix m
  return x

pinta :: (Estado -> Bool) -> Render () -> M ()
pinta esc ac = do 
  est <- leerM
  renderM $ do
    setAntialias (_antialias est)
    ponColor (_pincel est) (1-_transparencia est)
    setLineWidth (abs (_grosor est))
    let ds = _discontinua est
    {-when (not . null $ ds) $ -} 
    setDash ds 0
    ac
    if esc est then escala est (_pinta est) else _pinta est

pintaYRellena esc ac = do
  est <-leerM
  renderM $ case _relleno est of
    Nothing -> return ()
    Just c -> do
      ponColor c (1-_transparencia est)
      setLineWidth (abs (_grosor est))
      ac
      if esc est then escala est (_rellena est) else _rellena est
  pinta esc ac
  
escala est act = preservandoMatriz $ do 
 --Matrix scX _ _ scY _ _ <- getMatrix 
 scale (1/_scX est) (1/_scY est)
 act


escalaD (x,y) est = (x',y')
 where 
   x' = if x >= 0 then x else -x/_scX est
   y' = if y >= 0 then y else -y/_scY est
 
pro Nil                   = return ()
pro Point                 = do est <- leerM
                               renderM $ escala est $ do { (x,y) <- userToDevice 0 0
                                                         ; (x',y') <- deviceToUser (fromIntegral $ round x) (fromIntegral $ round y)
                                                         ; let y'' = 0.5+y'
                                                         ; setLineWidth 1
                                                         ; ponColor (_pincel est) (1-_transparencia est) 
                                                         ; moveTo x' y''
                                                         ; lineTo (x'+1) y''; stroke }
pro (Line (P x y))        = pinta (\est -> _grosor est < 0) $ do { moveTo 0 0; lineTo x y }
pro (Arc rx ry alfa beta) = do est <- leerM
                               pintaYRellena (\est -> _grosor est < 0) $ 
                                  preservandoMatriz $ do
                                   let (rx',ry') = escalaD (rx,ry) est 
                                   scale rx' ry'
                                   Graphics.Rendering.Cairo.arc 0 0 1 alfa beta
pro (Polygon [])          = return ()
pro (Polygon (P x y :ps)) = pintaYRellena  (\est -> _grosor est < 0) $ do 
                              moveTo x y        
                              mapM_ (\(P x y) -> lineTo x y) ps
                              closePath
pro (Text str)            = do est <- leerM
                                --pintaYRellena  (\est -> False) $ do                                 
                               renderM $  
                                  preservandoMatriz $ do
                                    let (f,t) = _fuente est 
                                    selectFontFace f FontSlantNormal FontWeightNormal
                                    setFontSize (abs t)  
                                    ponColor (_pincel est) 1
                                    Matrix a b c d e f <- getMatrix 
                                    setMatrix $ Matrix a b (-c) (-d) e f

                                    when (t<0) $ do 
                                      Matrix scX b c scY e f <- getMatrix
                                      setMatrix $ Matrix 1 0 0 1 e f  
                                      rotate (_rot est)
                                    extends <- textExtents str
          
                                    when (_textCentered est) $ do
                                      let w = textExtentsWidth extends + 2*textExtentsXbearing extends
                                      let h = textExtentsHeight extends + 2*textExtentsYbearing extends
                                      translate (-w/2) (-h/2) 
                                    showText str
                                    newPath
                                    --stroke
                                    --newPath 
                                    {-
                                    moveTo 0 (textExtentsYbearing extends)
                                    lineTo (w) (textExtentsYbearing extends)
                                    lineTo (w) (h+(textExtentsYbearing extends)) 
                                    lineTo (0) (h+(textExtentsYbearing extends))
                                    lineTo 0 (textExtentsYbearing extends)

                                    stroke 
                                    -}
                                    --newPath

                                    -- textPath str
                                    -- _rellena est
                                    -- textPath str
                                    -- _pinta est
                                    
                                    
pro (With ms g) = preservandoEstadoM $ do
  conM $ \est -> foldr apMod est ms
  pro g
pro (Transformed ts g) = preservandoEstadoM $ do
  m <- renderM getMatrix
  mapM_ apTransf ts
  pro g
  renderM $ setMatrix m
pro (g1 :> g2) = do
  pro g1
  pro g2
pro (Clip g1 g2) = do
  renderM save
  preservandoEstadoM $ do
    conM $ \est -> est{_pinta = do clip; newPath, _rellena = return ()}
    pro g1
  pro g2
  renderM restore                               
                         

apMod (Brush c)        est = est{_pincel=c}
apMod (Fill c)       est = est{_relleno=Just c} 
apMod (Transparency t) est = est{_transparencia=t} 
apMod (Thickness g)        est = est{_grosor=g} 
apMod (Dotted ds)  est = est{_discontinua=ds}
apMod (Font n t)      est = est{_fuente=(n,t)}
apMod (Antialias True)     est = est{_antialias=AntialiasNone}
apMod (Antialias False)     est = est{_antialias=AntialiasDefault}
apMod (TextCentered b)     est = est{_textCentered=b}



apTransf (Translate dx dy)  = do renderM $ translate dx dy
apTransf (Rotate alfa)      = do renderM $ rotate alfa; conM $ \est -> est{_rot=_rot est+alfa}
apTransf (Scale escX escY)  = do renderM $ scale escX escY; conM $ \est -> est{_scX=_scX est*escX, _scY=_scY est*escY} 



ponColor (C r g b) 0 = setSourceRGB r g b
ponColor (C r g b) a = setSourceRGBA r g b a


