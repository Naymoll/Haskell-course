fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)


-- data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
instance Enum Odd where
    succ (Odd x) = Odd (x + 2)
    pred (Odd x) = Odd (x - 2)

    enumFrom a = a : enumFrom (succ a)

    enumFromTo a'@(Odd a) b'@(Odd b) | a > b = []
                                     | otherwise = a' : enumFromTo (succ a') b' 

    enumFromThen a'@(Odd a) b'@(Odd b) | a == b = []
                                       | otherwise = a' : enumFromThen (Odd b) (Odd (b + b - a)) 

    enumFromThenTo a'@(Odd a) b'@(Odd b) c'@(Odd c) | a == c = [c']
                                                    | n' < 0 = []
                                                    | otherwise = f a' b' (n')
                                                        where 
                                                            f _ _ 0 = []
                                                            f x y'@(Odd y) n = x : f y' (Odd (y + len)) (n - 1)
                                                            n' = (c - a) `div` (b - a) + 1
                                                            len = b - a

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change x = [c : cs | c <- coins, c <= x, cs <- change (x - c)]