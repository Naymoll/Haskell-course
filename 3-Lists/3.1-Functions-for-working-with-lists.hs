import Data.List

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y lst = x : y : lst  

nTimes:: a -> Int -> [a]
nTimes val 0 = []
nTimes val n = val : nTimes val (n-1)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) | x `mod` 2 == 0 = oddsOnly xs
                  | otherwise      = x : oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = map sum $ transpose [xs,ys,zs]

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = (takeWhile (== head xs) xs) : groupElems (dropWhile (== head xs) xs)