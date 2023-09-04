module Queue 
  ( Queue , 
    emptyQ ,       --  Queue a
    isEmptyQ ,     --  Queue a -> Bool 
    addQ ,         --  a -> Queue a -> Queue a
    remQ           --  Queue a -> (  a , Queue a )
   ) where 

data Queue a = Qu [a] [a] deriving (Eq, Ord, Show)

emptyQ :: Queue a
emptyQ = Qu [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Qu [] []) = True
isEmptyQ _          = False

addQ   :: a -> Queue a -> Queue a
addQ x (Qu xs ys) = Qu xs (x:ys)

remQ   :: Queue a -> (  a , Queue a )
remQ (Qu [] [])       = error "remQ"
remQ (Qu (x:xs) ys)   = (x , Qu xs ys)
remQ (Qu [] ys)       = remQ (Qu (reverse ys) [])


