import Data.List
import Data.String
import Data.Int
import System.IO
import Prelude

-- Pure enumeration types. --

{- data Bool = False | True

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False -}

data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter

weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold

-- Simple product types --
-- The type constructor lack parameters
-- A product type is a combination of types.

data Person1 = Adult1 String String Int

nemo = Adult1 "Clownfish Nemo" "The Reef" 7
peter = Adult1 "Peter Pan" "Neverland" 13

sumAge1 :: [Person1] -> Int
sumAge1 [] = 0
sumAge1 ((Adult1 _ _ a) : xs) = a + sumAge1 xs

-- We could name types for increased readability

type Name = String
type Address = String
type Age = Int

data Person2 = Adult2 Name Address Age

nemo2 = Adult2 "Clownfish Nemo" "The Reef" 7
peter2 = Adult2 "Peter Pan" "Neverland" 13

sumAge2 :: [Person2] -> Age
sumAge2 [] = 0
sumAge2 ((Adult2 _ _ a) : xs) = a + sumAge2 xs

-- Union types --
-- More than one way value constructor.

data Shape = Circle Float | Rectangle Float Float

circle1 = Circle 3.0

area :: Shape -> Float
area(Circle r) = 3.14 * r * r
area(Rectangle w h) = w * h

-- Algebraic data types --
-- be both product and union types.
-- Has two constructors and second constructor has parameters.
-- Can also be polymorphic.
-- Can be recursively described.

type Price = Float 
data StoreItem t = Item t Price
-- t gives different types.

-- Item "book" 25.0 :: StoreItem String
-- Item (Circle 1.0) 1000.0 :: StoreItem Shape

data EXPR = Const Int 
            | Var String 
            | Op String EXPR EXPR
            | App String EXPR deriving (Eq, Ord, Show)

-- Recursive parameters, using the type itself.
-- deriving generate instance declaration for Eq, Ord, Show.
-- Automatic inclusion.

-- Monomorphic linear lists. --

data IntList = IntNil | IntCons Int IntList

-- Linear list of integers.
-- Two kinds:
-- : IntNil, empty list, every list ends with an IntNil.
-- : IntCons, non-empty list, everylist contains an integer and an
-- IntList

--  IntCons 2(IntCons 1 (IntCons 3 IntNil)) :: IntList

length2 :: IntList -> Int
length2 IntNil = 0
length2 (IntCons _ xs) = 1 + length2 xs

data List u = Nil | Cons u (List u)

-- A Linear list of values of type u
--  Two kinds:
-- : Nil, empty lists. Every empty list ends with a Nil
-- : Cons, non-empty lists. node = cons. every such node 
-- contains value of type u and a List u.

-- Monomorphic binary trees. --

data IntTree = IntEmpty | IntNode IntTree Int IntTree
-- The name itself occurs, therefore recursive. 

-- 2 childs, one inner node as INT 

node1 = IntNode (IntNode IntEmpty 4 (IntNode IntEmpty 2 IntEmpty)) 
        1 
        (IntNode IntEmpty 3 IntEmpty)

size_n :: IntTree -> Int
size_n IntEmpty = 0
size_n (IntNode left _ right) = 1 + size_n left + size_n right

-- Polymorphic binary trees. --

data Tree q = Empty | Node (Tree q) q (Tree q)

-- Values must be of the same type. 

tree1 = Node(Node Empty 4.0 (Node Empty 2.0 Empty))
        1.0
        (Node Empty 3.0 Empty) :: Tree Float

size_t :: Tree w -> Int
size_t Empty = 0
size_t (Node left _ right) = 1 + size_t left + size_t right

depthT :: Tree t -> Int
depthT Empty = 0
depthT (Node t1 _ t2) = 1 + max(depthT t1)(depthT t2)

collapse_t :: Tree d -> [d]
collapse_t Empty = []
collapse_t (Node t1 x t2) = collapse_t t1 ++ [x] ++ collapse_t t2

-- Mutually defined algebraic data types --
{-
    data EXPR = Const Int 
            | Var String 
            | Op String EXPR EXPR
            | App String EXPR deriving (Eq, Ord, Show)

    EXPR is a type for expression trees. 
    such are used to represent expressions in computer programs. 

    4 different kinds of EXPR Values 

    - Integer constants
    - Variables
    - Binary operations (+, -, *, /)
    - Function applications

    Recursive type. Union and product type (both).

    Op "/" (App "sin" (Var "x"))(Op "+" (Var "x")(Const 1))
    => (sin x) / (x + 1)
-}
  
expr1 = Op "/" (App "sin" (Var "x"))(Op "+" (Var "x")(Const 1)) :: EXPR

count :: EXPR -> Int
count (Const _) = 1
count (Var _) = 1
count (Op _ e1 e2) = 1 + count e1 + count e2
count (App _ e1) = 1 + count e1


