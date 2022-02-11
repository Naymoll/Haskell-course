module Lib where

import Data.List(partition, find, dropWhileEnd)
import Data.List.Split(splitOn)
import Data.Char(isDigit, isSpace)
import Data.Either(lefts)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

splitLines :: String -> [[String]]
splitLines str = map trimStrip $ lines str
    where trimStrip str = map (dropWhileEnd isSpace . dropWhile isSpace) (splitOn "=" str)

parseFields :: [[String]] -> Either [(String, String)] Error
parseFields f | all isField f   = Left $ map (\[k,v] -> (k, v)) f
              | otherwise       = Right ParsingError
    where isField [k, v] = not (null k) && not (null v)
          isField _      = False

collectField :: Either [(String, String)] Error -> Either (String, String, String) Error
collectField (Left f)  =
    let
        fN = findField "firstName" f
        sN = findField "lastName" f
        a  = findField "age" f
        l  = lefts [fN, sN, a]
    in
        if length l == 3 then Left (l !! 0, l !! 1, l !! 2) else Right IncompleteDataError
    where findField n f = maybe (Right IncompleteDataError) (\(_,v) -> Left v) (find (\(k,_) -> k == n) f)

collectField (Right e) = Right e

fromFields :: Either (String, String, String) Error -> Either Error Person
fromFields (Left (f, s, a)) | all isDigit a   = Right $ Person f s (read a)
                            | otherwise       = Left $ IncorrectDataError a

fromFields (Right e)         = Left e  

parsePerson :: String -> Either Error Person
parsePerson = fromFields.collectField.parseFields.splitLines