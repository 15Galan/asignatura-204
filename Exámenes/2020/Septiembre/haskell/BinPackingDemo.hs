{------------------------------------------------------------------------------
 - Data Structures, Grado en Informática. UMA.
 -
 - First Fit algorithm for Bin Packing problem
 -----------------------------------------------------------------------------}
 
import AVL

list1 :: [Weight]
list1 = [9, 9, 9, 9, 9, 9, 9, 9, 9]

list2 :: [Weight]
list2 = [5, 7, 5, 2, 4, 2, 5, 1, 6]

solve :: Capacity -> [Weight] -> [Bin]
solve w os = toList (addAll w os)

solveLinear :: Capacity -> [Weight] -> [Bin]
solveLinear w oo = seqToList (linearBinPacking w oo) 

main = do
  putStrLn ("Solving W = 10 and "++show list1)
  mapM_ print (solve 10 list1)
  putStrLn ("\n--------------------\n")  
  putStrLn ("Solving W = 10 and "++show list2)
  mapM_ print (solve 10 list2)
  
  {-
  -- Para alumnos sin evaluacion continua
  -- ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT

  
  putStrLn("LinearBinPacking W=10 and "++show list1)
  mapM_ print (solveLinear 10 list1)
  putStrLn ("\n--------------------\n")  
  putStrLn ("LinearBinPacking W=10 and "++show list2)
  mapM_ print (solveLinear 10 list2)
  -}