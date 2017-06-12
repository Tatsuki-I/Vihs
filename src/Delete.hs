module Delete where

deleteLine :: [String] -> Int -> Int ->[String]
deleteLine str line times
  | line <= length str && line >= 0 && line - 1 <= length str =
    take (line - 1) str ++ (reverse . take (length str - line - times + 1) $ reverse str)
  | otherwise = str
{-
deleteLine str line n | line <= length str && line >= 0 && line + n - 1 <= length str = (take line str) ++ (reverse $ take ((length str) - line - n) $ reverse str)
-}

{-
main = do
    print $ deleteLine str 1 1
    where
        str = ["aaa", "bbb", "ccc"]
-}
