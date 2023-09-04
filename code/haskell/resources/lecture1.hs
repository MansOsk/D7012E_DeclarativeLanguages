threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z 
    |  (x == y) = False
    |  (y == z) = False
    |  (z == x) = False
    | otherwise = True

threeEqual :: Int -> Int -> Int -> Bool
threeEqual x y z
    |  (x == y && y == z && z == x) = True
    |   otherwise = False 

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual x y z a
    sum = x * y / z
    | (threeEqual x y z) = True
    | a == sum = True
    | otherwise = False
