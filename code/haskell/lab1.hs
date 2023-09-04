-- Måns Oskarsson - mnsosk-7@student.ltu.se

import Data.List
import Data.String
import Data.Int
import System.IO

--- MAIN_FUNCTIONALITY ---

smallestKsets :: [Int] -> Int -> IO()  -- OUT !!! -> IO()
smallestKsets [] k = error "Empty List!"
smallestKsets x k = printAnswer (listToTuple (takeSmallestK k (sortList (generateSets (x) 1)))) 

generateSets :: [Int] -> Int -> [(Int, Int, Int, [Int])]
generateSets [] _ = []
generateSets (x:xs) i = [(getSize (splitBeforeIndex j (x:xs)),   i,   (i + (j-1)),  splitBeforeIndex j (x:xs)) | j <- [1..y]] ++ (generateSets xs (i + 1)) -- FORMAT: SIZE I J [SUBLIST]
    where
        y = getLength (x:xs)

sortList :: [(Int,Int,Int,[Int])] -> [(Int, Int, Int, [Int])]
sortList [] = []
sortList (x:xs) = mergeList x (sortList xs)

mergeList :: (Int,Int,Int,[Int]) -> [(Int, Int, Int, [Int])] -> [(Int,Int,Int,[Int])] 
mergeList x []  = [(x)]
mergeList x (y:ys) 
    |  getTupleWeight(x) < getTupleWeight(y) = x:(y:ys) 
    |  otherwise = y:mergeList x ys

takeSmallestK :: Int -> [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
takeSmallestK _ [] = []
takeSmallestK k (x:xs)
    |   k <= 0 = []
    |   otherwise = [x] ++ takeSmallestK (k - 1) xs 

--sortByWeight:: [(Int, Int, Int,[Int])] -> [(Int, Int, Int,[Int])]

--- LIST_FUNCTIONALITY ---

getSize :: [Int] -> Int
getSize [] = 0
getSize (x:xs) = x + getSize xs

getLength :: [Int] -> Int
getLength [] = 0
getLength (x:xs) = 1 + getLength xs

getIndex :: Int -> [Int] -> Int
getIndex _ [] = -1
getIndex i x 
    | getIndexBackw i x == -1 = -1
    | otherwise = (getLength x) - (getIndexBackw i x) + 1

getIndexBackw :: Int -> [Int] -> Int
getIndexBackw i (x:xs) 
    | xs == [] = -1
    | x == i =  getLength $ [x] ++ xs
    | otherwise = getIndexBackw i xs

-- Distinct numbers only! !
getLast :: [Int] -> Int
getLast [] = -1
getLast (x:xs)
    | xs == [] = x 
    | otherwise = getLast xs

-- Disticnt numbers only!!
removeItem :: Int -> [Int] -> [Int]
removeItem _ [] = []
removeItem x (y:ys) 
    | x == y    = ys
    | otherwise = y : removeItem x ys

removeItemOnIndex :: Int -> [Int] -> [Int]
removeItemOnIndex _ [] = []
removeItemOnIndex x (y:ys) 
    | x == 1    = ys
    | otherwise = y : removeItemOnIndex (x - 1) ys 

splitBeforeIndex :: Int -> [Int] -> [Int]
splitBeforeIndex _ [] = []
splitBeforeIndex i (x:xs)
    | getLength (x:xs) <= i = (x:xs)
    | otherwise = splitBeforeIndex i ((removeItemOnIndex (getLength $ x:xs) (x:xs))) 

--- TUPLES_FUNCTIONALITY ---

getTupleWeight :: (Int, Int, Int, [Int]) -> Int
getTupleWeight (w,_,_,_) = w

--- MISC_FUNCTIONALITY ---
printAnswer :: String -> IO()
printAnswer x = putStr $ ("\n" ++ "size" ++ "\t" ++ "i" ++ "\t" ++ "j" ++ "\t" ++ "sublist" ++ "\n" ++ x ++ "\n")

printLine :: String -> IO ()
printLine x = putStr $ x ++ "\n"

printPairLine :: String -> String -> IO()
printPairLine x y = putStr $ x ++ y ++ "\n"

listToTuple :: [(Int, Int, Int, [Int])] -> String
listToTuple [] = []
listToTuple (x:xs) = tupleToString x ++ "\n" ++ listToTuple xs

tupleToString :: (Int, Int, Int, [Int]) -> String
tupleToString (a,b,c,d) = show a ++ "\t" ++ show b ++ "\t" ++ show c ++ "\t" ++ show d 
 
main = do 
    --smallestKsets [x*(-1)^x | x <- [1..100]] 15
    smallestKsets [24,-11,-34,42,-24,7,-19,21] 6
    smallestKsets  [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] 8
