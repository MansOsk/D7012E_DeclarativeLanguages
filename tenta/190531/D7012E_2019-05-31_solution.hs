-- Suggested solution 
-- Declarative languages D7012E 2019-05-31
-- /Håkan Jonsson

-- 190531: 1a

import Data.Int (Int)
totalSum :: [Int] -> Int
totalSum xs = foldr (+) 0 xs

{-
*Main> totalSum [1,2,3,4]
10
*Main>
-}

-- 190531: 1b

f1 x y = y x

f2 = map (.)

f3 f x y = f (f x) y

{-
*Main> :type f1
f1 :: t1 -> (t1 -> t2) -> t2
*Main> :type f2
f2 :: [b -> c] -> [(a -> b) -> a -> c]
*Main> :type f3
f3 :: ((t1 -> t2) -> t1 -> t2) -> (t1 -> t2) -> t1 -> t2
*Main> 
-}

-- 190531: 2

tr :: [[t]] -> [[t]]
tr [x] = foldr (\a ra -> [a] : ra) [] x
tr (xs:xss) = map (\(g,h)->g:h) (zip xs (tr xss))

-- Other versions

tr2 [] = []
tr2 ([]:_) = []
tr2 (x:xs) = tr2' (x:xs) : tr2 (map tail (x:xs))
  where
    tr2' [] = []
    tr2' (x:xs) = head x : tr2' xs

tr3 [] = []
tr3 (x:xs) = tr3' (tr3'' x) (tr3 xs)
  where
    tr3'' [] = []
    tr3'' (x:xs) = [x] : tr3'' xs
    tr3' [] [] = []
    tr3' (x:xs) (y:ys) = (x++y) : tr3' xs ys
    tr3' xs [] = xs


{-
*Main> tr [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
[[1,5,9],[2,6,10],[3,7,11],[4,8,12]]
*Main> 
-}

-- 190531: 3

triangle :: Int -> IO ()
triangle 0 = return ()
triangle n = do putStrLn (replicate n '*')
                triangle (n-1)

{-
*Main> triangle 4
****
***
**
*
*Main> 
-}

-- Other versions

triangle2 :: Int -> IO ()
triangle2 0 = putStr ""
triangle2 n = putStr (replicate n '*' ++ "\n") >> triangle (n-1)

-- triangle3 builds upon the fact that there is no input handling, so
-- the output can be computed in an ordinary (pure) Haskell function (rows)
-- that the main function (triangle3) just prints
triangle3 :: Int -> IO ()
triangle3 n = putStrLn (rows n)
  where
    rows 1 = "*"
    rows n = replicate n '*' ++ "\n" ++ rows (n-1)
    
-- 190531: 4

mergeUnq [] b = b
mergeUnq a [] = a
mergeUnq (a:as) (b:bs)
   | a < b     = a : mergeUnq as (b:bs)
   | b < a     = b : mergeUnq (a:as) bs
   | otherwise = mergeUnq (a:as) bs
   
ham :: [Int]
ham = 1 : mergeUnq
           (map (2*) ham)
           (mergeUnq (map (3*) ham) (map (5*) ham))




list = [1..] : map (map (+1)) list
{-
take 20 $ map (\ (a:b:_) -> (a,b)) list

*Main> :type list
list :: [[Integer]]
*Main> 
-}





  

-- while' p s f = if p s then while' p (f s) f else s

{-
*Main> while' (\(n,s)->n>0) (5,0) (\(n,s)->(n-1,s+n)) 
(0,15)
*Main> 
-}