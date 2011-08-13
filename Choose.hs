module Choose where

combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

combinations k n = combinationsOf k [1..n]
