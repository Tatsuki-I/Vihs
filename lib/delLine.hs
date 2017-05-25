deleteLine :: [String] -> Int -> [String]
deleteLine str n | n > length str = str
                 | otherwise = (init $ take n str) ++ (reverse $ take ((length str) - n) $ reverse str)

main = do
    print str
    print $ deleteLine str 2
    where
        str = ["abc", "def", "ghi"]
