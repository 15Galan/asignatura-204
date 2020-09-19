cerosUnos :: Integer -> (Integer,Integer)
cerosUnos 0 = (1,0)
cerosUnos 1 = (0,1)
cerosUnos x | mod x 10 == 1 = (fst (cerosUnos num), 1 + snd (cerosUnos num))
            | mod x 10 == 0 = (1 + fst (cerosUnos num), snd (cerosUnos num))
            | otherwise     = error "¿Eso es un número binario?"
    where
        num   = div x 10
