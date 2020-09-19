pam :: [a -> b] -> a -> [b]
pam [ ] _ = [ ]
pam (f:fs) x = f x : pam fs x

{-
pam [\x -> x+1, \x -> 2*x, \x -> x*x] 3 => [4, 6, 8]
	(+1) = \x -> x+1
	(*2) = \x -> 2*x
	(*)  = \x -> x*x

pam [(+1), (*2), (*)] 3 => [4, 6, 8]
-}