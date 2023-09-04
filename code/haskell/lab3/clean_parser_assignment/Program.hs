module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T] 
instance Parse T where
  parse = createStates
  toString = makeString 

createStates :: Parser T
createStates = iter Statement.parse >-> createProgram
createProgram = Program
             
makeString  :: T -> String
makeString (Program []) = []
makeString  (Program statement) = Statement.makeString  (head statement) "" ++ makeString  (Program (tail statement))
          
exec :: T -> [Integer] -> [Integer]
exec (Program statements) xs = Statement.exec statements Dictionary.empty xs
