module Stack 
  ( Stack , 
    new ,       --  Stack a
    empty ,     --  Stack a -> Bool 
    push ,      --  a -> Stack a -> Stack a
    pop         --  Stack a -> (  a , Stack a )
   ) where -- after this the functionality is implemented

data Stack a = S [a] -- comment next line to get real ADT
   deriving (Eq, Ord, Show)

new :: Stack a
new = S []

empty :: Stack a -> Bool
empty (S []) = True
empty _      = False

push   :: a -> Stack a -> Stack a
push x (S xs) = S (x:xs)

pop   :: Stack a -> (  a , Stack a )
pop (S [])       = error "pop"
pop (S (x:xs))   = (x , S xs)

