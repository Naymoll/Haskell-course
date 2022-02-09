data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) 

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * r ^ 2 
area (Rectangle w h) = w * h

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle w h) = w == h
isSquare (Circle _)      = False

data Result' = Success' | Fail' Int

instance Show Result' where
    show Success'  = "Success"
    show (Fail' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' d = case doSomeWork d of 
    (Success, _) -> Success'
    (Fail, n)    -> (Fail' n)


data Bit = Zero | One

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One  = 1

instance Show Bit where
    show Zero = "0"
    show One  = "1"


data Sign = Minus | Plus deriving Eq
instance Show Sign where
  show Minus = "-"
  show Plus  = "+"

zToInt :: Z -> Int 
zToInt (Z sign bits) | sign == Minus = -helper bits
                     | otherwise     = helper bits 
    where helper []     = 0
          helper (x:xs) = bitToInt x + 2 * helper xs

intToZ :: Int -> Z
intToZ n | n >= 0    = Z Plus (helper n)
         | otherwise = Z Minus (helper (abs n)) 
    where helper 0 = []
          helper n | n `mod` 2 == 1 = [One] ++ helper (n `div` 2)
                   | n `mod` 2 == 0 = [Zero] ++ helper (n `div` 2)

data Z = Z Sign [Bit] deriving Show

add :: Z -> Z -> Z
add l r = intToZ $ (zToInt l) + (zToInt r)

mul :: Z -> Z -> Z
mul l r = intToZ $ (zToInt l) * (zToInt r)