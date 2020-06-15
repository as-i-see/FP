module Main where

testArray1 = []
testArray2 = [1]
testArray3 = [1, 3, 4, 2, 8, 5, 5, 9, 7, 1]
testArray4 = [1, 3, 4, 4, 2, 8, 5, 9, 7, 3, 5, 3, 7, 9, 2, 6, 7]
testArray5 = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

findTwo :: [Int] -> Int
findTwo [] = -1
findTwo [one] = -1
findTwo [first, second] = if first == second then first else -1
findTwo (first:other) = if first == (head other) then first else findTwo other

main :: IO ()
main = do
    print (findTwo testArray1)
    print (findTwo testArray2)
    print (findTwo testArray3)
    print (findTwo testArray4)
    print (findTwo testArray5)
