class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageGork a == True && doesEnrageMork a == True = stomp.stab $ a
                  | doesEnrageGork a == True = stab a
                  | doesEnrageMork a == True = stomp a
                  | otherwise = a

class (Enum a, Bounded a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a | a == maxBound = minBound
            | otherwise = succ a

    spred :: a -> a
    spred a | a == minBound = maxBound
            | otherwise = pred a

avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger (a' + b' + c') / 3.0
    where a' = toInteger a
          b' = toInteger b
          c' = toInteger c