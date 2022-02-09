doubleFact :: Integer -> Integer
doubleFact n | n == 0 || n == 1 = 1
             | otherwise = n * doubleFact(n - 2)

fibonacci :: Integer -> Integer
fibonacci n | n ==  0   =  0
            | n ==  1   =  1
            | n < 0     = fibonacci(n + 2) - fibonacci(n + 1)
            | otherwise = fibonacci(n - 1) + fibonacci(n - 2)

fibonacci' :: Integer -> Integer
fibonacci' n = fibHelp n 0 1

fibHelp :: Integer -> Integer -> Integer -> Integer
fibHelp n val1 val2 | n == 0    = val1
                    | n == 1    = val2
                    | n < 0     = fibHelp (n + 1) val2 (val1 - val2)
                    | otherwise = fibHelp (n - 1) val2 (val1 + val2) 