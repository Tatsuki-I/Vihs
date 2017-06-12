module Insert where

insert :: String -> String -> Int -> String
insert buff c x = take x buff ++ c ++ reverse (take (length buff - x) (reverse buff))

insertBuff :: [String] -> String -> Int -> Int -> [String]
insertBuff buff c x y = take y buff ++ [insert (buff !! y) c x] ++ reverse (take (length buff - y - 1) (reverse buff))

main :: IO ()
main = do
    print $ take 1 str
    print $ insertBuff str "b" 1 2
    where
        str = ["AAA", "BBB", "CCC"]
