concatList :: [[a]] -> [a]
concatList = foldr' (++) []

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f ini [] = ini
foldr' f ini (x:xs) = x `f` foldr f ini xs

lengthList :: [a] -> Int
lengthList = foldr (\x s -> 1 + s) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0