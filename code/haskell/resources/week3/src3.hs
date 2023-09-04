import Data.List
import Data.String
import Data.Int
import System.IO
import Prelude

-- Abstract data types (ADT:s) --

-- A data type is abstract when the external API and 
-- the internal implementation is decoupled

-- As long as the API is unaffected, we are allowed to reprogram 
-- the internal implementation as we like.

-- Keyword: module
-- set of declarations.
-- specifies which decl. are visible outside the module (external)
-- Can be imported (used) by other modules. 


-- Lazy evaluation --

{- 
    Haskell systems evaluate only
    a) what is needed and
    b) as late as possible
    everything is evaluated " on demand" only

    * efficient
    * admits infinite data structures

    * subtopics
    overall evaluation strategy
    pattern matching
    guards
    list comprehension
    refutable patterns 
    infinite data structures
-}

{-
    Overall evaluation strategy

    Rule 1, needed only.
    each arguments to a function is evaluated only if needed 
    for continuation.

    Rule 2, needed parts only
    only the parts of a compound argument that are needed
    are evaluated.

    Rule 3, once only
    each argument is evaluated at most once.
    the evaluated value is stored and reused later, if the argument
    occurs again in the evaluation. 

    operators are functions and their evaluations therefore lazy

    Rule 4, "ouside in"
    f1 e1 (f2 e2 17)

    starts with f1 then f2i f neccesary

    Rule 5, "left - right"
    f1 e1 (+) f2 e2 where (+) binary operator
    its start with f1 e1.
-}

{-
    Pattern matching

    pattern matching is done "top-down, left-right"
    - function application
    - case expressions

-}

{-
    Evaluation of guards 

    guards are only evaluated in the case where the pattern 
    matching succeds

    top-down
    Pattern matching in each case but not in the individual guards.
-}

{-
    Local definitions

    Calls local definitions only when needed!!

    refutable patterns.
-}

{-
    Data directed programming

    laziness is good for efficiency

    Example 1: Find the sum of the fourth powers of all 
    numbers 1,2,...,n.

    Solution.

    Build the list 1..n
    construct a new list [1,16,...,n^4]
    compute the sum of this list

    not as inefficient as one might think
-}

sum_f :: [Int] -> Int
sum_f  [] = 0
sum_f  (x : xs) = x + sum_f  xs

map_f :: (a->b) -> [a] -> [b]
map_f f []      = []
map_f f (x:xs)  = f x : map_f f xs 

sumFourthPowers :: Int -> Int
sumFourthPowers n = sum_f (map_f (^4) [1..n]) 
 -- sum_f drives the data
 -- IMPORTANT: No intermediate list created!


-- Problem: Compute the minimum of a list.
-- Solution: Sort the list and return the head.
-- head . iSort
-- In fact, the list is never constructed if we just ask for the
    -- head

iSort :: [Int] -> [Int]
iSort []        = []
iSort (x:xs)    = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
    | x <= y    = x:(y:ys)
    | otherwise = y:ins x ys

{-
    Infinite data structures

    Enabled by laziness.
    Only evalutaed when needed, finite part. 

    Infinite lists can always be used, but only a part of data!!!
-}

-- the infinite list of ones
ones = 1 : ones

nat :: [Int] -- all natural numbers
nat = 0 : [ x+1 | x <- nat ]

int :: [Int] -- all integers, listed from 0
int = 0 : (concat.tail) [ [x,-x] | x <- nat ]

powers :: Int -> [Int]
powers n = [ n^k | k <- [0,1 .. ]]

primes :: [Int]
primes  =  sieve [2 .. ]
  where
     sieve (x:xs)  =  
        x : sieve [ y | y <- xs , y `mod` x > 0]

rnd :: Int -> [Int]
rnd seed = next : rnd next
  where
    next = (multiplier * seed + increment) `mod` modulus
    modulus = 31
    multiplier = 17
    increment = 23

pascal :: [[Int]]
pascal = [1] : map (\xs -> zipWith (+) ([0] ++ xs) (xs ++ [0])) pascal

merge' [] ys = ys
merge' xs [] = xs
merge' (x:a) (y:b) | x<y   = x : merge' a (y:b)
                   | x==y  = y : merge' a b 
                   | x>y   = y : merge' (x:a) b

-- all numbers whose prime factors are only (1,) 2, 3, and 5

twos = 1:map (*2) [1..]
threes = 1:map (*3) [1..]
fives = 1:map (*5) [1..]

ham = merge' twos (merge' threes fives)