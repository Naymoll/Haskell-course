data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil         = []
fromList (Cons x xs) = x : fromList xs 

toList :: [a] -> List a
toList []     = Nil
toList (x:xs) = Cons x $ toList xs

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc $ toNat (n-1)

add :: Nat -> Nat -> Nat
add a b = toNat $ fromNat a + fromNat b 

mul :: Nat -> Nat -> Nat
mul a b = toNat $ fromNat a * fromNat b 

fac :: Nat -> Nat
fac x = toNat $ product [1..fromNat x] 

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf x)   = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf x)   = 1
size (Node a b) = 1 + size a + size b 

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1, x)
    go (Node a b) = 
        let 
            aN = go a
            bN = go b
        in
            (,) (fst aN + fst bN) (snd aN + snd bN)