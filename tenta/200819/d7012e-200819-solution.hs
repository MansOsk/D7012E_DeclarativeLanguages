-- Suggested solutions Haskell D7012E 2020-08-19 /Håkan Jonsson, LTU

{-
*Main> :type tail "7"
tail "7" :: [Char]
*Main> :type []:[]:[]
[]:[]:[] :: [[a]]
*Main> :typ length.map (+1)
length.map (+1) :: Num a => [a] -> Int
*Main> :type \h -> 'h' 0

<interactive>:1:7: error:
    • Couldn't match expected type ‘Integer -> t’
                  with actual type ‘Char’
    • The function ‘'h'’ is applied to one argument,
      but its type ‘Char’ has none
      In the expression: 'h' 0
      In the expression: \ h -> 'h' 0
*Main> :type map map
map map :: [a -> b] -> [[a] -> [b]]
*Main>
-}

ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

{-
*Main> ack 0 2
3
*Main> ack 2 0
3
*Main> ack 1 1
3
*Main> 
-}

-- -------------------------------------------------------

middle :: [a] -> a
middle [] =  error "no middle in an empty list"
middle ls = head (drop num ls)
  where
    num = (length ls - 1) `div` 2

-- without predefined functions
middle2 [] = error "no middle in an empty list"
middle2 ls = middle2helper ls ((length ls +1) `div` 2)
  where
    middle2helper (x:xs) 1 = x
    middle2helper (x:xs) n = middle2helper xs (n-1) 

-- *Main> middle [1]
-- 1
-- *Main> middle2 [1]
-- 1
-- *Main> middle [1,2,3]
-- 2
-- *Main> middle2 [1,2,3]
-- 2
-- *Main> middle [1,2,3,4]
-- 2
-- *Main> middle2 [1,2,3,4]
-- 2
-- *Main> middle [1,2,3,4,5]
-- 3
-- *Main> middle2 [1,2,3,4,5]
-- 3
-- *Main> middle [1,2,3,4,5,6]
-- 3
-- *Main> middle2 [1,2,3,4,5,6]
-- 3
-- *Main>

qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort ls =  qsort left ++ mid ++ qsort right
  where
    m = middle ls
    left =  [x|x <- ls, x < m]
    mid =   [x|x <- ls, x == m] -- to preserve duplicates in ls 
    right = [x|x <- ls, x> m]

-- *Main> qsort [3,1,4,2,5]
-- [1,2,3,4,5]
-- *Main> qsort [1.0, 4.0, 2.0, 1.0, 5.0, 4.0, 3.0]
-- [1.0,1.0,2.0,3.0,4.0,4.0,5.0]
-- *Main>

mkList :: Num t => t -> t -> [t]
mkList f s = f : mkList (f+d) (s+d)
  where d = s-f

-- *Main> take 10 (mkList 1 (-4))
-- [1,-4,-9,-14,-19,-24,-29,-34,-39,-44]
-- *Main> take 10 (mkList 1 1)
-- [1,1,1,1,1,1,1,1,1,1]
-- *Main> take 10 (mkList 1 4)
-- [1,4,7,10,13,16,19,22,25,28]
-- *Main> 

-- -------------------------------------------------------

data Tree a = Leaf | Node (a,Int) (Tree a)  (Tree a) (Tree a) deriving (Eq, Show)

t = Node ('X',1)
          (Node ('H',2)
                Leaf
                Leaf
                Leaf
          )
          Leaf
          (Node ('Z',4)
                (Node ('Y',3)
                      Leaf
                      Leaf
                      Leaf
                     )
                (Node ('J',2)
                      Leaf
                      Leaf
                      Leaf
                     )
                Leaf
          )

topPart :: Int -> Tree a -> [a]
topPart 0 _ = []
topPart _ Leaf = []
topPart i (Node p t1 t2 t3)  = fst p : ra
  where ra = topPart (i-1) t1 ++ topPart (i-1) t2 ++ topPart (i-1) t3

-- strings are returned below because t contains chars; a list of char
-- is a string in Haskell
-- 
-- *Main> topPart 1 t
-- "X"
-- *Main> topPart 2 t
-- "XHZ"
-- *Main> topPart 3 t
-- "XHZYJ"
-- *Main> topPart 4 t
-- "XHZYJ"
-- *Main>

-- -------------------------------------------------------

calc :: IO ()
calc = do
         putStr "Enter number of positive integers to add: "
         line <- getLine
         sum <- calc' (read line) 0
         putStr ("Total sum: " ++ (show sum) ++ "\n")
  
calc' :: Int -> Int -> IO Int
calc' 0 sum = return sum
calc' n sum = do
                putStr "Enter an integer: "
                next <- getLine
                calc' (n-1) (sum + read next)

-- *Main> calc
-- Enter number of positive integers to add: 4
-- Enter an integer: 3
-- Enter an integer: -2
-- Enter an integer: 4
-- Enter an integer: 1
-- Total sum: 6
-- *Main> 
-- *Main> calc
-- Enter number of positive integers to add: 3
-- Enter an integer: 1
-- Enter an integer: 0
-- Enter an integer: -1
-- Total sum: 0
-- *Main>

