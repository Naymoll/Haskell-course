import Data.Char(isDigit)
import Data.List(find)
import Data.List.Split

data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2 

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2-x1) + abs (y2-y1)

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord x y) = Coord (w * fromIntegral x + w / 2.0) (w * fromIntegral y + w / 2.0)

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = Coord (floor $ x / w) (floor $ y / w)

findDigit :: [Char] -> Maybe Char
findDigit = find isDigit

findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of 
    Just c  -> c
    Nothing -> 'X'

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = [] 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe x  = Just (head x)
