import Data.List
import Data.String
import Data.Int
import Data.Char
import System.IO
import Prelude 

-- 14.1

data Temp   = Cold | Hot
              deriving (Eq,Ord,Enum,Show,Read)
data Season = Spring | Summer | Autumn | Winter 
              deriving (Eq,Ord,Enum,Show,Read) 


weather :: Season -> Temp
weather x = if x == Summer then Hot else Cold

-- 14.4  14.5 

data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float Float
             deriving (Eq,Ord,Show,Read) 

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _ ) = False
isRound (Triangle _ _ _ _) = False 

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w
area (Triangle b h _ _) = b * h / 2 

perimeter :: Shape -> Float
perimeter (Circle r) = pi * r
perimeter (Rectangle h w) = 2*h + 2*w 
perimeter (Triangle b _ b' c') = b * b' * c'

-- 14.6

regular_ :: Shape -> Bool
regular_ (Circle _) = True
regular_ (Rectangle h w) = h == w
regular_ (Triangle a _ b c) = a == b && b == c 

-- 14.15 14.16 14.17 14.18

data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr |
            Div Expr Expr 

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2 

show_ :: Expr -> String
show_ (Lit n) = show n
show_ (Add e1 e2) = show_ e1 ++ "+" ++ "(" ++ show_ e2 ++ ")"
show_ (Sub e1 e2) = show_ e1 ++ "-" ++ "(" ++ show_ e2 ++ ")"
show_ (Mul e1 e2) = show_ e1 ++ "*" ++ "(" ++ show_ e2 ++ ")"
show_ (Div e1 e2) = "(" ++ show_ e1 ++ ")" ++ "/" ++ show_ e2

size :: Expr -> Int
size (Lit n) = 0
size (Add e1 e2) = size e1 + size e2 + 1
size (Sub e1 e2) = size e1 + size e2 + 1
size (Mul e1 e2) = size e1 + size e2 + 1
size (Div e1 e2) = size e1 + size e2 + 1
    

-- 14.21

data NTree = NilT |
             Node Int NTree NTree 
             deriving (Show,Read) 

getSubtrees :: NTree -> (NTree,NTree)
getSubtrees (Node n ls rs) = (ls, rs) 

-- 14.21 - 14.24

exists::NTree -> Int -> Bool
exists NilT _ = False
exists (Node i l r) v
    | i == v = True
    | otherwise = exists l v || exists r v

getMax::NTree -> Int
getMax NilT = minBound
getMax (Node i l r) = max i (max (getMax l) (getMax r))

getMin::NTree -> Int
getMin NilT = maxBound
getMin (Node i l r) = min i (min (getMin l) (getMin r))

reflect::NTree -> NTree
reflect NilT = NilT
reflect (Node i l r) = Node i r l

-- 14.33

data GTree a = Null | Nod a (GTree a) (GTree a)
    deriving (Show, Read, Eq)

countGT :: GTree a -> Int
countGT Null  = 1
countGT (Nod _ x y) = countGT x + countGT y 

depthGT :: GTree a -> Int
depthGT Null = 0
depthGT (Nod _ x y) = max (1 + depthGT x) (1 + depthGT y)

sumGT :: GTree Int -> Int
sumGT Null = 0
sumGT (Nod n x y) = n + (sumGT x) + (sumGT y)

existsGT :: Eq a => GTree a -> a -> Bool
existsGT Null _ = False
existsGT (Nod n x y) i
        |  n == i    = True
        |  otherwise = existsGT x i || existsGT y i

mapTree:: (a->b) -> GTree a -> GTree b
mapTree _ Null = Null
mapTree f (Nod i l r) = Nod (f i) (mapTree f l) (mapTree f r)

flatten::GTree a -> [a]
flatten Null = []
flatten (Nod i l r) = [i] ++ flatten l ++ flatten r

-- 17.1


-- [x+y | x <- [1 .. 4], y <- [2 .. 4], x > y]
-- x = 1 y = 2 is x > y. No. continue
-- x = 2 y = 2 is x > y. No. continue
-- x = 3 y = 2 is x > y. Yes. x+y = 5 needed parts only. Continue
-- x = 4 y = 2 is x > y. Yes. x+y = 6. Continue.
-- x = 1 y = 3 is x > y. No. continue
-- ...
-- ...
-- x = 4 y is 3, yes. x+y = 7. continue.
-- x = 1 y = 4....
-- ...
-- ...
-- End

-- 17.2

-- 17.22


