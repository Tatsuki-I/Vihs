{-
module Delete where
-}
deleteLine :: [String] -> Int -> Int ->[String]
deleteLine str line n | line <= length str && line >= 0 && line + n - 1 <= length str = (take line str) ++ (reverse . take ((length str) - line - n) $ reverse str)
{-
deleteLine str line n | line <= length str && line >= 0 && line + n - 1 <= length str = (take line str) ++ (reverse $ take ((length str) - line - n) $ reverse str)
-}
                      | otherwise = str

main = do
    print $ deleteLine str 1 1
    where
        str = ["aaa", "bbb", "ccc"]

