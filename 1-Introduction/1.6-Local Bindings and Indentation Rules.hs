seqA :: Integer -> Integer
seqA n = let helper n frst scnd thrd 
                      | n == 0 = frst
                      | n == 1 = scnd
                      | n == 2 = thrd
                      | otherwise = helper (n - 1) (scnd) (thrd) (thrd + scnd - 2 * frst)
         in 
             helper n 1 2 3

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x < 0 = sum'n'count $ abs x
              | otherwise = (a x, b x)
                    where
                        b x | x < 10 = 1
                            | otherwise = 1 + b (x `div` 10)
                        a x | x < 10 = x
                            | otherwise = a (x `div` 10) + x `mod` 10

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a == b = 0
                    | otherwise = h * ((f a + f b) / 2 + sum xs) 
                        where
                            n = 1000
                            h = (b - a) / n
                            xs = map f [a + h, a + h * 2 .. b - h]