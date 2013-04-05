module Choose where

combinationsOf :: (Eq b, Num b) => b -> [a] -> [[a]]
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

combinations :: (Enum a, Num a, Eq b, Num b) => b -> a -> [[a]]
combinations k n = combinationsOf k [0..n-1]
