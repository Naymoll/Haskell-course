lastElem :: [a] -> a
lastElem = foldl1 (flip const)

revRange :: (Char,Char) -> [Char]
revRange (l, r) = unfoldr helper r
    where helper c | c >= l    = Just(c, pred c)
                   | otherwise = Nothing