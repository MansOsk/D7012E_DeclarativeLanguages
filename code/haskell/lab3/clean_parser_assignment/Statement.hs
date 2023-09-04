module Statement(T, parse, toString, fromString, exec, makeString) where
import Prelude hiding (return, fail, read, repeat)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Skip                        |
    Begin [Statement]           |
    While Expr.T Statement      |
    Read String                 |
    Write Expr.T                |
    Assignment String Expr.T    |
    Repeat Expr.T Statement     |
    If Expr.T Statement Statement
    deriving Show

skip, begin, while, read, write, assignment, iff, repeat :: Parser Statement

---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

skip = (accept "skip" #- require ";") >-> makeSkip
makeSkip x = Skip 

begin = ((accept "begin") -# (iter parse) #- (require "end")) >-> makeBegin
makeBegin x = Begin x

while = ((accept "while" -# Expr.parse #- require "do") # parse) >-> makeWhile
makeWhile (x,y) = While (x) (y)

read = (accept "read" -# word #- require ";") >-> makeRead
makeRead x = Read x

write = (accept "write" -# Expr.parse #- require ";") >-> makeWrite
makeWrite x = Write x

iff = (accept "if" -# Expr.parse) # (require "then" -# parse #- require "else") # parse>-> makeIf
makeIf ((x,y),z) = If x y z

repeat = (accept "repeat" -# Expr.parse #- require "until" # parse #- require ";") >-> makeRepeat
makeRepeat (x,y) = Repeat y x

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

exec [] dict input = []

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec(Skip:stmts) dict input = exec (stmts) dict input

exec(Begin statements:stmts) dict input = exec (statements ++ stmts) dict input

exec(While cond statements: stmts) dict input = 
    if(Expr.value cond dict) > 0
        then exec (statements : While cond statements : stmts) dict input
        else exec (stmts) dict input

exec(Read statement:stmts) dict input = exec stmts (Dictionary.insert (statement, head input) dict) (tail input)

exec(Write statement:stmts) dict input = Expr.value statement dict : exec stmts dict input

exec (Assignment str exp:stmts) dict input = exec stmts (Dictionary.insert (str,Expr.value exp dict) dict) input

exec (Repeat cond statements:stmts) dict input =
 if (Expr.value cond dict) <= 0
 then exec (statements : Repeat cond statements : stmts) dict input   
 else exec (stmts) dict input

---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

makeString :: T -> String -> String
makeString (Skip) s = s ++ "skip;\n"
makeString (Begin xs) s = s ++ "begin\n" ++ foldl (++) []  (map (\x-> makeString x (" " ++ s)) xs) ++ s ++ "end\n"
makeString (While cond state) s = s ++ "while " ++ toString cond ++ " do\n"++ (makeString state (" " ++ s)) 
makeString (If cond y z) s = s ++ "if " ++ toString cond ++ " then\n"  ++ (makeString y (" " ++ s)) ++ s ++"else\n"  ++ (makeString z (" " ++ s))
makeString (Read str) s = s ++ "read " ++ str ++ ";\n"
makeString (Write exp) s = s ++ "write " ++ toString exp ++ ";\n" 
makeString (Assignment str exp) s = s ++ str ++ ":=" ++ toString exp ++ ";\n"
makeString (Repeat x y ) s = s ++ "repeat\n" ++ (makeString y (s ++ " ")) ++ "until " ++ toString x ++ ";\n"


---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

instance Parse Statement where
  parse = skip ! begin ! while ! read ! write ! assignment ! iff ! repeat
  toString = (\x-> makeString x "") 
