permutation :: Integer -> Integer -> Integer
permutation n r = div (fact n) $ fact (n - r)
    where
        fact x = product [1..x]