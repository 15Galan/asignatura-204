module FoldStack where
import qualified DataStructures.Stack.LinearStack as S

foldrStack :: (a -> b -> b) -> b -> S.Stack a -> b
foldrStack f z s | S.isEmpty s = z
                 | otherwise   = S.top s `f` foldrStack f z (S.pop s)

listToStack :: [a] -> S.Stack a
listToStack (x:xs) = foldr (\y x -> insert x b) [] (x:xs)
    where
        b = S.empty

stackToList :: S.Stack a -> [a]
stackToList s = foldrStack undefined