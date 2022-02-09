meanList :: [Double] -> Double
meanList = mean.foldr (\x (s,c) -> (x + s, c + 1)) (0,0)
    where 
        mean (0, 0) = 0
        mean (s, c) = s / c

evenOnly :: [a] -> [a]
evenOnly = reverse.fst.foldl f ([], 1) 
    where
        f = (\(xs, pos) x -> if even pos then (x : xs, pos + 1) else (xs, pos + 1))

evenOnly' = foldr (\(i, x) xs -> if even i then x:xs else xs) [] . zip [1..]