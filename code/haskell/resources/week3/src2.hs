import Data.List
import Data.String
import Data.Int
import System.IO
import Prelude

-- Error handling -- 

-- Mix error with productive code, in efficient.

-- Alternative 1
-- error construct.
-- error :: String -> a
-- Immediately terminates the program.
errDiv1 :: Int -> Int -> Int
errDiv1 n m
    | (m /= 0)  = n `div` m
    | otherwise = error "Div by 0"  -- Error

-- Alternative2: Function return a value of an error type
-- type Maybe

data Maybe a = Nothing | Just a
            deriving (Eq,Ord,Show)

-- errDiv2 :: Int -> Int -> Maybe Int
-- errDiv2 n m 
--   | (m /= 0) = Just (n `div` m)
--   | otherwise = Nothing 

-- Map maybe

-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- mapMaybe _ Nothing = Nothing
-- mapMaybe g (Just x ) = Just (g x )

-- maybe :: b  ->  (a -> b)  ->  Maybe  a -> b
-- maybe  n  _  Nothing  =  n
-- maybe  _  f  (Just x)  =  f x

-- Algebraic data types and type classes --

data Vector = Vec Float Float

class Movable a where
  	move      		:: Vector -> a -> a
  	reflectX  		:: a -> a
  	reflectY  		:: a -> a
  	rotate180	:: a -> a
  	rotate180 = reflectX . reflectY

data Point = Point Float Float 
             deriving Show

-- Declaring movable with help of point
-- Instance is "object"
instance Movable Point where
	move (Vec v1 v2) (Point c1 c2) = 
		Point (c1+v1) (c2+v2)
  	reflectX (Point c1 c2)  = Point c1 (-c2)
  	reflectY (Point c1 c2)  = Point (-c1) c2
  	rotate180 (Point c1 c2) = Point (-c1) (-c2)


data Figure = Line Point Point |
              Circle2 Point Float 
              deriving Show

-- Declaring movable with help of data type Figure. 
instance Movable Figure where
  	move v (Line p1 p2) = 
		Line (move v p1) (move v p2)
  	move v (Circle2 p r) = Circle2 (move v p) r
	reflectX (Line p1 p2) = 
		Line (reflectX p1) (reflectX p2)
	reflectX (Circle2 p r) = Circle2 (reflectX p) r
	reflectY (Line p1 p2) = 
		Line (reflectY p1) (reflectY p2)
	reflectY (Circle2 p r) = Circle2 (reflectY p) r