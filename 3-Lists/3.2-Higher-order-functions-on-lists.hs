import Data.Char

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p l xs = filter (\x -> p x || l x) xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let left = qsort [a | a <- xs, a <= x]
        right = qsort [a | a <- xs, a > x]
    in  left ++ [x] ++ right

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [y | p <- perms xs, y <- interleave p]
  where
    interleave []     = [[x]]
    interleave (y:ys) = (x:y:ys) : map (y:) (interleave ys)

delAllUpper :: String -> String
delAllUpper = unwords.filter (not.all isUpper).words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 xs ys zs = zipWith3 (\x y z -> max (max x y) z) xs ys zs 