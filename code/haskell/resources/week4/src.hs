-- Chapter 18
import Data.List
import Data.String
import Data.Int
import System.IO
import Prelude


-- IO MONADS --

--  I/O in haskell is problematic, might cause sideeffects.
-- diff = readInt - readInt to be 0, this need not be true.

-- IO is performed by small atomic io actions lined up in sequence.
-- an io action has a type IO, where a is the type of the value 
-- action creates.

-- getLine :: IO String
-- 'do' construct importnt for sequential io reading and writing.

-- Function putStr writes strings.
-- () is a value of type () used when nothing really needs 
-- to be returned from a function.

-- do construct
-- imperative execution. executed top-down one line at a time.

-- Capturing IO results in do contructor. 

getNput :: IO ()
getNput = do line <- getLine
             putStrLn line

-- read two lines and reverse. 
reverse2lines :: IO ()
reverse2lines
  = do line1 <- getLine
       line2 <- getLine
       putStrLn (reverse line2)
       putStrLn (reverse line1)

-- <- is a generator construct.
-- digs out the result of an action
-- can only be used in do constructs.
-- values cant be reassigned.

-- we can also make local decl. in do construct. 
-- with key word let
reverse2lines' :: IO ()
reverse2lines'
  = do line1 <- getLine
       line2 <- getLine
       let rev1 = reverse line1
       let rev2 = reverse line2
       putStrLn rev2
       putStrLn rev1

-- Haskell has no built-in support for iteration.
-- instead we simulate iteration with recursion.
-- if we need a lot of iteration, we define general
-- helper functions.

-- while: repeats an io action while a condition io bool creates
-- a true. 

while :: IO Bool -> IO () -> IO ()
while test action
  = do res <- test
       if res then do action
                      while test action
              else return ()

-- an example of an io bool is end of file. 
-- isEOF :: IO Bool 

copyInputToOutput :: IO ()
copyInputToOutput
  = while (do res <- isEOF
              return (not res))
          (do line <- getLine
              putStrLn line)

myRead :: IO ()
myRead = while (do putStr "Enter line ==>"
                   line <- getLine
                   return (line /= ""))
               (do putStr "Thanks!\n")

goUntilEmpty' :: IO ()
goUntilEmpty'
 = do line <- getLine
      while (return (line /= []))
            (do putStrLn line
                line <- getLine
                return ())

goUntilEmpty :: IO ()
goUntilEmpty
  = do line <- getLine
       if (line == [])
          then return () 
          else (do putStrLn line
                   goUntilEmpty)

getInt :: IO Int
getInt = do line <- getLine
            return (read line)
    
sumInts :: IO Int
sumInts
  = do n <- getInt
       if n==0 
          then return 0
          else (do m <- sumInts
                   return (n+m))

sumInteract :: IO ()
sumInteract
  = do putStrLn "Enter integers one per line"
       putStrLn "These will be summed until zero is entered"
       sum <- sumInts
       putStr "The sum is "
       print sum

-- FILE IO
-- HASKELL HAS IO, readfile, writefile, appendfile.