import Data.List
import Data.String
import Data.Int
import Data.Char
import System.IO
import Prelude hiding (return, fail)
import Text.Parsec (between)

-- Question 9.1

-- List comprehension 
doubleAll1 :: [Int] -> [Int]
doubleAll1 x = [2*n | n <- x]

-- Primitive recursion
double2Helper :: Int -> [Int]
double2Helper x = [x*2]

doubleAll2 :: [Int] -> [Int]
doubleAll2 [] = []
doubleAll2 (x:xs) = double2Helper x ++ (doubleAll2 xs)

-- Using Map
doubleAll3 :: [Int] -> [Int]
doubleAll3 xs = map (\y -> y*2) xs

-- Question 9.2

myLength :: [a] -> Int
myLength [] = 0
myLength xs = sum (map (\ x -> 1) xs)

myLength2 :: [a] -> Int
myLength2 xs = foldr (+) 0 (map (\_ -> 1)  xs)

-- Question 9.4

addOne :: Int -> Int
addOne x = x + 1

-- Question 9.6
square_ :: [Int] -> [Int]
square_ lst = map (\n -> n^2) lst

sumSquare :: [Int] -> Int
sumSquare s = sum (square_ s)

greater :: [Int] -> Bool 
greater x = length ls == length x
    where ls = filter (>0) x

-- Question 9.7

-- Give the minimum value of a function f on inputs 0 to n
minFunc :: (Int -> Int) -> Int -> Int
minFunc f 0 = f 0 
minFunc f n 
    | b < a             = b
    | otherwise         = a 
        where a = f n
              b = minFunc f (n-1)

eqFunc :: (Int -> Int) -> Int -> Bool 
eqFunc _ 0 = True
eqFunc f n 
    | f n == f (n-1)    = eqFunc f (n-1)
    | otherwise         = False 

gthanzeroFunc :: (Int -> Int) -> Int -> Bool
gthanzeroFunc _ 0 = True
gthanzeroFunc f n
    | a > 0     = gthanzeroFunc f (n-1)
    | otherwise = False
            where a = f n

genList :: (Int -> Int) -> Int -> [Int]
genList f n = [f x | x <- [0..n]]

incrementalHelper :: [Int] -> Bool
incrementalHelper [] = True
incrementalHelper (x:xs)
    | xs == []      = True
    | x < (head xs) = incrementalHelper xs 
    | otherwise     = False 


incrementalFunc :: (Int -> Int) -> Int -> Bool 
incrementalFunc f n = incrementalHelper (genList f n)

-- Question 9.9

iter :: Int -> (a -> a) -> a -> a
iter 0 _ n = n
iter i f n = f $ iter (i-1) f n

-- Questin 9.10
double :: Int -> Int
double x = x*2

two_n :: Int -> Int 
two_n n = iter n double 2 

-- Questin 9.11

nat_square :: Int -> Int
nat_square 0 = 0
nat_square n = foldr (+) 0 $ map (\x -> x^2) ls
        where ls = [0..n]

-- Question 9.16

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst f (x:xs)
    |   f x     = x : filterFirst f xs 
    |   otherwise = xs

-- Question 9.17

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast f x = reverse $ filterFirst f (reverse x)

-- Question 10.3

composeList :: [a -> a] -> a -> a
composeList (x:xs) = foldr (.) x xs 

-- Question 10.7

add2 = \x -> (\y -> x+y)
sub2 = \x -> (\y -> x-y)

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f = (\x y -> f y x)

-- Question 10.8
-- Char -> Bool
-- Using not and elem 
-- not :: Bool -> Bool. Invert true to false.
-- elem :: a -> [a] -> Bool. True if any element in list [a] equals to a 

isValidChar :: Char -> Bool
isValidChar = \c -> (not (elem c " \t\n"))

-- Question 12.2
numEqual :: Eq a => a -> [a] -> Int
numEqual x ls = length [n | n <- ls, n == x]

member :: Eq a => [a] -> a -> Bool
member = \x y -> (numEqual y x > 0)

-- Question 12.3
oneLookupFirst :: Eq a => [(a,b)] -> a -> b
oneLookupFirst ((x,y):xs) n
    |   x == n    = y
    |   otherwise = oneLookupFirst xs n


oneLookupSecond :: Eq b => [(a,b)] -> b -> a
oneLookupSecond ((x,y):xs) n
    |   y == n    = x
    |   otherwise = oneLookupSecond xs n

-- Question 12.5

class Visible a where
    toString :: a -> String
    size     :: a -> Int 

instance Visible Integer where
    toString int = show int

instance Visible Char where
    toString ch = [ch]
    size      _ = 1 

-- Question 12.6

compare_ :: Visible a => a -> a -> Bool
compare_ x y = size x <= size y

-- Question 13.1
f' n = 37+n
f True = 34

g 0 = 37
g' n = True

--h x
     -- | x > 0        = True
     -- | otherwise    = 37

k x = 34
k' 0 = 35

-- Question 13.2

-- (Int -> b) (a -> Bool) can be unified to Int -> Bool

-- (Int, a, a) (a, a,[Bool]) can be unified since output from first function
-- Is of ok type of both arguments of second tripplet.

-- Question 13.4

-- f :: (a,[a]) -> b
-- No since type a only accepts arguments of the same arbitrary type.

-- Question 13.5

-- Same here 

-- Question 13.6

f_ :: [a]->[b]->a->b
f_ (x:_) (y:_) z = y

h :: [b] -> b -> b
h x = f_ x x 
-- h x = f x x