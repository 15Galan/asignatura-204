-------------------------------------------------------------------------------
-- Data Structures. Grado en Informática. UMA.
-- @ Pepe Gallardo
--
-- Library for random number generation.
-- Uses the algorithm in standard Java random library.
--
-------------------------------------------------------------------------------

module DataStructures.Util.Random
  ( Seed
  , Random(randoms,randomsR)
  , randomsIn
  ) where

import Data.Array
import Data.Bits
import Data.Char
import Data.Int
import Data.Word

type Seed = Int

randomsIn :: [a] -> Seed -> [a]
randomsIn xs s = map (a!) $ aleatInt l s
 where
  l = length xs
  a = listArray (0,l-1) xs

{-
-- Devuelve una lista infinita de randoms en el intervalo [l,h], s es la Seed
uniforme :: (Random a) => (a, a) -> Int -> [a]
uniforme (l,h) s = randomRs (l,h) (mkStdGen $ v s)



-- Devuelve una lista de n booleanos. La probabilidad de True es p. s es la Seed
bernoulli :: (Ord a, Fractional a, Random a) => Int -> a -> Int -> [Bool]
bernoulli n p s = map (<p) . take n . randoms . mkStdGen $ s

normal :: (Ord a, Floating a, Random a) => Int -> a -> a -> Int -> [a]
normal n media desv s = take n . map gauss . pairs . randoms . mkStdGen $ s
 where
   pairs (x:y:zs) = (x,y) : pairs zs
   gauss (r1,r2) = media + desv * sqrt (-2 * log r1) * cos (2 * pi * r2)

-}



setSeed :: Word64 -> Word64
setSeed s = (s `xor` 0x5DEECE66D) .&. mask

a, c :: Word64
a = 0x5DEECE66D
c = 0xB

iter64 :: Word64  -> [Word64]
iter64 s = {- drop 2 $ -} iter' (setSeed s)
 where
  iter' :: Word64  -> [Word64]
  iter' s =  y `seq` y : (iter' $! s')
   where
    s' = f s .&. mask
    y  = s'
    f x = x*a + c


iter :: Int -> Word64  -> [Int]
iter bits s = {- drop 2 $ -} iter' (setSeed s)
 where
  sh = 48-bits
  iter' :: Word64  -> [Int ]
  iter' s =  y `seq` y : (iter' $! s')
   where
    s' = f s .&. mask
    y = fromIntegral $ shiftR s' sh
    f x = x*a + c


mask :: Word64 
mask = shift 1 48 - 1


-- entero aleatorio
aleatAnyInt :: Int -> [Int]
aleatAnyInt s = map fromIntegral (iter 32 $ fromIntegral s)

-- en [0,n-1]
aleatInt :: Int -> Int -> [Int]
aleatInt n s
 | esPot2 n = pot2 . iter 31 $ fromIntegral s
 | otherwise = pasa . iter 31 $ fromIntegral s
 where

   pot2 (b:bs) = b' `seq` b' : pot2 bs
    where b' = aInt (shiftR (aWord64 b * aWord64 n) 31)

   nMas1 = n+1
   pasa (b:bs)
    | b - v + nMas1 < 0 = pasa bs
    | otherwise         = v : pasa bs
    where
      v = mod b n

esPot2 :: Int -> Bool
esPot2 n = (n .&. (-n)) == n


aleatBool :: Int -> [Bool]
aleatBool s = aBools . iter 1 $ fromIntegral s
 where
   aBools (x:xs) = y `seq` y : aBools xs
    where y = x/=0

-- en [0,1)
aleatFloat :: Int -> [Float]
aleatFloat s = aFloats (iter 24 $ fromIntegral s)
 where
   aFloats (x:xs) = z `seq` z : aFloats xs
    where z = aFloat x / uno24

uno24 :: Float
uno24 = aFloat (shiftL 1 24)

-- en [0,1)
aleatDouble :: Int -> [Double]
aleatDouble s = aDoubles (iter64 $ fromIntegral s)
 where
   aDoubles (x:y:xs) = z `seq` z : aDoubles xs
    where z = aDouble (shiftL (sh x 26) 27 + sh y 27) / uno53
          sh :: Word64 -> Int -> Word64
          sh x b =  shiftR x (48-b)

uno53 :: Double
uno53 = aDouble (shiftL 1 53)

aDouble :: Word64 -> Double
aDouble = fromIntegral

aFloat :: Int -> Float
aFloat = fromIntegral

aWord64 :: Int -> Word64
aWord64 = fromIntegral

aInt :: Word64 -> Int
aInt = fromIntegral

aleatoriosRInt :: (Int,Int) -> Int -> [Int]
aleatoriosRInt (l,h) s = map (+l) $ aleatInt n s
  where n = h-l+1

aleatoriosREnum :: (Enum a) => (a, a) -> Int -> [a]
aleatoriosREnum (l,h) s = map toEnum $ aleatoriosRInt (fromEnum l,fromEnum h) s


class Random a where
  randoms :: Seed -> [a]
  randomsR :: (a,a) -> Seed -> [a]

instance Random Bool where
  randoms = aleatBool
  randomsR = aleatoriosREnum

instance Random Char where
  randoms s = map chr $ aleatInt (1 + ord(maxBound :: Char)) s
  randomsR = aleatoriosREnum

instance Random Int where
  randoms = aleatAnyInt
  randomsR = aleatoriosRInt

instance Random Integer where
  -- El aleatoriosRInt es el de Int !!!
  randoms = map toInteger . aleatAnyInt
  randomsR (l,h) s = map (iValInteger (l,h) . toInteger) (aleatAnyInt s)

iLogBase :: Integer -> Integer -> Integer
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)

instance Random Double where
  randoms = aleatDouble
  randomsR (0,1) s = aleatDouble s
  randomsR (l,h) s = map (iValDouble (l,h) id) (aleatAnyInt s)

instance Random Float where
  randoms = aleatFloat
  randomsR (0,1) s = aleatFloat s
  randomsR (l,h) s = map (iValDouble (realToFrac l, realToFrac h) realToFrac) (aleatAnyInt s)


iValInteger :: (Integral a) => (Integer,Integer) -> a -> Integer
iValInteger (l,h) v = fromInteger (l + toInteger v `mod` k)
          where
           k = h - l + 1
           b = 2147483561
           n = iLogBase b k

iValDouble :: (Num a) => (Double, Double) -> (Double -> a) -> Int -> a
iValDouble (l,h) fromDouble x = scaled_x'
  where x' = iValInteger (toInteger (minBound::Int32), toInteger (maxBound::Int32)) x
        scaled_x' = fromDouble ((l+h)/2) +
                    fromDouble ((h-l) / realToFrac int32Range) *
                    fromIntegral x'


int32Range :: Integer
int32Range = toInteger (maxBound::Int32) - toInteger (minBound::Int32)



