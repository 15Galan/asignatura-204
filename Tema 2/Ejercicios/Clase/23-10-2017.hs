fib n = fibAux n 1 1
    where
        fibAux 0 x y = y
        fibAux n x y = fibAux (n-1) (x+y) x

{-
fib' n = fibAux n 0 1
    where
        fibAux 0 i j = j
        fibAux n i j = fibAux (n-1) (i+j) i
-}

take' :: Int -> [a] -> [a]
take' _ [ ] = [ ]
take' 0 xs  = [ ]
take' n (x:xs) | n >= 0    = x : (take' (n-1) xs)
               | otherwise = error "Error en el valor n."


drop' :: Int -> [a] -> [a]
drop' 0 xs  = xs
drop' n [ ] = [ ]
drop' n (x:xs) = drop (n-1) xs

-- Hacer en casa takeWhile' y dropWhile'