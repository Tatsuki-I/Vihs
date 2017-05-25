import ReadWrite
import Delete

main = do
    str <- createBuffer "test.txt"
    print str
    print $ deleteLine str 2 2
    buffToFile "test2.txt" $ deleteLine str 2 2
