-- General tutorial: https://youtu.be/02_H3LjqMr8?t=2909
{-
This is a 
block comment
-}


-- :t sqrt in ghci. checking square root function in haskell! Very good inspecting pre-built functions. 

import Data.List
import System.IO

maxInt = maxBound :: Int

sumOfNums = sum [1..1000] -- List 1....1000 sum of all numbers in list.

---------------------------------------   MATH  --------------------------------------------------------

addEx = 5 + 4 
subEx = 5 - 4 
multEx = 5 * 4 
divEx = 5 / 4 

modEx = mod 5 4    -- Prefix operator
modEx2 = 5 `mod` 4     -- Infix operator

negNumEx = 5 + (-4)     -- Need perentaces since confuses compiler when adding negative numbers. 

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9) -- From intergral convert integer to floating point. Sqrt is float function pre in haskell. 

-- Built in math functions
piVal = pi
ePow9 = exp 9 
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

-- Also sin, cos, tan, asin, atan, acos, sinh, asinh etc... 

trueAndFalse = True && False
trueOrFalse = True ||False
notTrue = not(True)

---------------------------------------  LIST  --------------------------------------------------------

primeNumbers = [3,5,7,11]
morePrimes = primeNumbers ++ [13,17,19,23,29]

favNums = 2 : 7 : 21 : 66 : []  -- Combine numbers into a list!!

multList = [[3,5,7],[11,13,17]]

morePrimes2 = 2 : morePrimes  -- Add infront of list

------------ Diverse functions-------------

lenPrime = length morePrimes2  -- Length of list

revPrime = reverse morePrimes2

isListEmpty = null morePrimes2  -- Check if empty

secondPrime = morePrimes2 !! 1  -- Index 2 of list.

firstPrime = head morePrimes2

lastPrime = last morePrimes2

initPrime = init morePrimes2 -- Anything except very last value

first3Primes = take 3 morePrimes2 -- Take first three values.

removedPrimes = drop 3 morePrimes2 -- Drop first three values.

is7InList = 7 `elem` morePrimes2  -- Check if 7 is in list

maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2

--------------------------------------------

newList = [2,3,5]
prodPrimes = product newList

zeroToTen = [0..10]

evenList = [2,4..20]

letterList = ['A','C'..'Z']

infinPow10 = [10,20..]

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cycleList = take 10 (cycle [1,2,3,4,5])

listTimes2 = [x * 2 | x <- [1..10]]

listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]

divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9,3,2,6,899,123,33,4]

sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThen5 = filter (>5) morePrimes

evensUpTo20 = takeWhile (<= 20) [2,4..]

multOfList = foldl (*) 1 [2,3,4,5]  -- Does operation on all items in list resulting in single.

------------ List comprahension -------------

pow3List = [3^n | n <- [1..10]]

multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

------------        Tuples     -------------

randTuple = (1, "Random Tuple")

bobSmith = ("Bob Smith", 52)

bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]
namesNAddresses = zip names addresses

---------------------------------------  FUNCTIONS  --------------------------------------------------------

{-
main = do 
    putStrLn "Whats your name"
    name <- getLine
    putStrLn ("Hello " ++ name)
-}

-- funcName param1 param2 = operations (returned value)
-- funcName param1 param2 = operations (returned value)
-- funcName param1 param2 = operations (returned value)

addMe :: Int -> Int -> Int
addMe x y = x + y

sumMe x y = x + y   -- No types declared.

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x,y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge x = "Nothing important"

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial(n - 1)

prodFact n = product [1..n]


isOdd :: Int -> Bool
isOdd n 
    |  n `mod` 2 == 0 = False
    | otherwise = True

isEven n = n `mod` 2 == 0


whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age > 6) && (age <= 10) = "Elementary school"
    | (age > 10) && (age <= 14) = "Middle school"


batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Avg"
    | avg <= 0.250 = "Avg player"
    | avg <= 0.280 = "Youre doing pretty good"
    | otherwise = "superstar"
    where avg = hits / atBats


getListItems :: [Int] -> String
getListItems [] = "Empty list"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest are" ++ show xs


getFirstItem :: String -> String
getFirstItem [] = "Empty string"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

---------------- Higher order functions ---------------------

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs    -- !!!!!!!!!!!!!! x:xs where x is current xs is the rest. Kind of recursion...

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys 
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int       -- Except a function to be passed in this function whos is INT.

doMult func = func 3

num3Times4 = doMult times4  -- Pass into doMult

getAddFunc :: Int -> (Int -> Int)   -- Return function with Int

getAddFunc x y = x + y 

adds3 = getAddFunc 3

fourPlus3 = adds3 4

threePlusList = map adds3 [1,2,3,4,5]

---------------- Lambdas ---------------------

dbl1To10 = map (\x -> x * 2 )[1..10]

---------------------------------------  LOGIC  --------------------------------------------------------

-- < > <= >= == /=
-- && || not 

doubleEvenNumber y =        -- if then else
    if (y `mod` 2 /= 0)
        then y 
        else y * 2

getClass :: Int -> String

getClass n = case n of      -- Case  / Switch 
    5 -> "Kindergarten"
    6 -> "Elementary"
    _ -> "Go away"

---------------------------------------  IMPORTING CUSTOM MODULES  --------------------------------------------------------

-- module SampFunctions(getClass, doubleEvenNumber) where -- Parameter is function included in module. module is importable.
-- Here you declare functions

-- import SampFunctions

---------------------------------------  NUMERATIONS   --------------------------------------------------------

data BaseballPlayer = Pitcher | Catcher | Infielder | Outfield deriving Show    -- ENUMS

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOF = print(barryBonds Outfield)

---------------- Struct ---------------------

data Customer = Customer String String Double
    deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double

getBalance (Customer _ _ b) = b


data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats Rock"
shoot Rock Scissors = "Rock beats scissor"
shoot _ _ = " Error "


data Shape = Circle Float Float Float | Rectangle Float Float Float Float 
    deriving Show

area :: Shape -> Float

area (Circle _ _ r ) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)  -- Dollar sign is instead of perenteces. It priorites operation before

sumValue = putStrLn (show (1 +2))

sumValue2 = putStrLn . show $ 1 + 2 -- Simplfied version of above

----------------- Type classes ---------------------

-- Corrsepond to set of types. Operations defined for them

data Employee = Employee {name :: String, position :: String, idNum :: Int} deriving (Eq, Show)

samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 123}
pamMarx = Employee {name = "Pam Marx", position = "Sales", idNum = 1234444}

isSamPam = samSmith == pamMarx

samSmithData = show samSmith


data ShirtSize = S | M | L

instance Eq ShirtSize where
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

instance Show ShirtSize where
    show S = "Small"
    show M = "Medium"
    show L = "Large"

smallAvail = S `elem` [S, M, L]

theSize = show S


class MyEq a where
    areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
    areEqual S S = True
    areEqual M M = True
    areEqual L L = True
    areEqual _ _ = False

newSize = areEqual M M 


---------------------------------------  INPUT AND OUTPUT   --------------------------------------------------------


sayHello = do                       -- File I/O
    putStrLn "Whats your name"
    name <- getLine
    putStrLn $ "Hello " ++ name

writeToFile = do                       -- File O/I
    theFile <- openFile "test.txt" WriteMode
    hPutStrLn theFile ("Random line of text")
    hClose theFile

readFromFile = do
    theFile2 <- openFile "test.txt" ReadMode
    contents <- hGetContents theFile2
    putStr contents
    hClose theFile2



---------------------------------------  MISC  --------------------------------------------------------

fib - 1 : 1 : [a+b |(a,b) <- zip fib(tail fib)]