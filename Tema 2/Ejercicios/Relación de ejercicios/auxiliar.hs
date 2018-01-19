-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería [Informática].
-- Alumno: GALÁN HERRERA, ANTONIO J.
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: 07 / 40
-------------------------------------------------------------------------------
import Test.QuickCheck
import Data.List


---------------------------------------
-- Ejercicio 16
---------------------------------------

-- a)
múltiplosDe :: Integer -> [Integer]
múltiplosDe 0 = [0]
múltiplosDe x | x > 0     = [ x*m | m <- [1..]]
              | otherwise = [ ]