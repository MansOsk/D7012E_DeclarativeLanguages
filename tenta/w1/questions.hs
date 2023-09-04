import Data.List
import Data.String
import Data.Int
import System.IO
import Prelude hiding (return, fail)
import Text.Parsec (between)
import Language.Haskell.TH.Lib (match)

-- Question 3.7
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p
    | m == p = False
    | m == p = False
    | n == p = False
    | otherwise = True

-- Question 3.8
threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m==n) && (n==p)

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p s = threeEqual m n p && s == m

-- Question 3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
    | b^2 > 4.0 * a * c = 2
    | b^2 == 4.0* a * c = 1
    | b^2 < 4.0 * a * c = 0

-- Question 3.16
-- numberRoots :: Float -> Float -> Float -> Int 

-- Question 3.17

bothRoot :: Float -> Float -> Float -> (Float, Float)
bothRoot a b c = (  ((-1 * b) + sqrt(b^2 - 4 * a * c)) / (2 * a),
                        ((-1 * b) - sqrt(b^2 - 4 * a * c)) / (2 * a)    )

-- Question 4.7

multiRec :: Int -> Int -> Int
multiRec _ 0  = 0
multiRec 0 _  = 0
multiRec a b = multiRec a (b-1) + a

-- Question 4.8

sqrtHelper :: Int -> Int -> Int
sqrtHelper a b
    | multiRec a a <= b = a
    | otherwise = sqrtHelper (a-1) b

sqrtRec :: Int -> Int
sqrtRec a = sqrtHelper a a

-- Question 4.9

function_f :: Int -> Int
function_f 0 = 0
function_f a = function_f (a-1) + func_f a

func_f :: Int -> Int
func_f 0 = 0
func_f 1 = 44
func_f 2 = 17
func_f _ = 0

-- Question 4.14

powerOfTwo :: Int -> Int
powerOfTwo n
    | n == 0    = 1
    | otherwise = powerOfTwo (n `div` 2) * powerOfTwo (n `div` 2)
                  * (if n `mod` 2 == 1 then 2 else 1)

-- Question 5.2

minThree :: (Int, Int, Int) -> Int
minThree (x,y,z)
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z

betweenNumber :: Int -> Int -> Int -> Bool
betweenNumber x y z = x <= y && y <= z

midn :: (Int, Int, Int) -> Int
midn (0,0,0) = 0
midn (x,y,z)
    |   betweenNumber y x z = x
    |   betweenNumber z x y = x
    |   betweenNumber x y z = y
    |   betweenNumber z y x = y
    |   otherwise = z

maxThree :: (Int, Int, Int) -> Int
maxThree (x,y,z)
    | x >= y && x >= z  = x
    | y >= x && y >= z  = y
    | otherwise         = z

orderTriple :: (Int,Int,Int) -> (Int, Int, Int)
orderTriple a = (minThree a, midn a, maxThree a)

-- Question 5.10
divisor :: Int -> Int -> Bool
divisor x i
    | mod x i == 0 = True
    | otherwise = False

divisors :: Int -> [Int]
divisors n = [y | y <- [1,2 .. n], divisor n y]

-- Question 5.11
isMatching :: Int -> Int -> Bool
isMatching x y = x == y

matches :: Int -> [Int] -> [Int]
matches _ [] = []
matches n (x:xs)
    | isMatching n x = [x] ++ matches n xs
    | otherwise = matches n xs

-- Question 5.18

-- shift :: ((Int,Int),Int) -> (Int,(Int,Int))  
-- shift ((x,y),z) = (x,(y,z))

-- Question 5.22

onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines (x:xs) = show x ++ "\n" ++ onSeparateLines xs

-- Question 5.23 

duplicate :: String -> Int -> String
duplicate _ 0 = "\n"
duplicate s n = s ++ duplicate s (n-1)

-- Question 7.4

product_ :: [Int] -> Int
product_ [] = 1 
product_ (x:xs) = foldr (+) x xs

-- Question 7.7
ifUnique :: Int -> Int -> [Int] -> Bool
ifUnique _ _ [] = True
ifUnique n x (y:ys)
    |  x >= 2    = False
    |  n == y    = ifUnique n (x+1) ys
    |  otherwise = ifUnique n x ys

unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) = [n | n <- (x:xs), ifUnique n 0 (x:xs)]


main = do
    putStrLn("Result: ")
    print (unique [1,1,2,3,4,4,5,5])