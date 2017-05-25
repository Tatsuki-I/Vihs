import ReadWrite
import Delete

main = do
    str <- createBuffer "test.txt"
    print str
    print $ deleteLine str 0 1
    print $ deleteLine str 1 1
    print $ deleteLine str 2 2
    print $ deleteLine str 3 1
    print $ deleteLine str 4 3
