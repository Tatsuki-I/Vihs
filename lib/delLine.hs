deleteLine :: [String] -> Int -> Int ->[String]
deleteLine str line n | line <= length str && line > 0 && line + n - 1 <= length str = (take (line - n) str) ++ (reverse $ take ((length str) - line) $ reverse str)
                      | otherwise = str

main = do
    print str
    print $ deleteLine str 0 1
    print $ deleteLine str 1 1
    print $ deleteLine str 2 2
    print $ deleteLine str 3 1
    print $ deleteLine str 4 3
    where
        str = ["abc", "def", "ghi", "jkl", "mno"]
