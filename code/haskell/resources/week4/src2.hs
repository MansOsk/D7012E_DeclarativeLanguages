import Data.List
import Data.String
import Data.Char
import Data.Int
import System.IO
import Prelude hiding (return, iterate, fail)

-- "Parsing" --

{-
    A parser is a program that takes a string of chars,
    and produces some form of data structure that makes
    the syntatic structure of the string explicit

    Parsing is all about syntax.
    Compilers typically parses source files. 
    Compilers are built around toolsets/rules, how to form programs. 
    Parser uses these toolsets/rules.

    The data structure is usually some form of tree

    The production is governed by a set of precise syntax rules
    A grammar. 

    Our grammars are given in Backus-Naur-form (BNF)

    A set of derivation rules expressed with
    Meta symbols 
    Terminal symbols
    Non-terminal symbols.

    There are different BNF:s and we will keep it simple.
-}


-- Parser programs --

{-
    Reads one character at a time from left to right. 
    The input is assumed to be a string

    The production rules of the grammar controls how the characters
    are read and are interpreted. 

    Tries to form syntactiaclly correct sentences and represent
    them by a tree, in our case, a value of an algebraic data type.

    In practice, we program small parsers for all basic parts in valid
    sentences

    We then combine such parsers into parser sequences to parse more
    complicated parts. 
-}

-- Parser a, lab assignment H3 --
{-
    Parser operators build a parser from small parsers.
    Solve problems with subproblems, construct arithemtic operators,
    then use it for bigger problems.

    Parser a:
-}

type Parser a = String -> Maybe (a, String)
-- Parser a is a type. Parser a is type of function that can generate
-- operations based on string.

data Maybe t = Nothing | Just t -- Nothing = incorrect, inside Just is
                                -- is successful parser

-- The argument of the parser is the string form which characters are
-- consumed
-- The result is the parsed a and what remains of the string.

star  ::  String -> Maybe (Char, String) 
star  ::  Parser Char
star ('*' : xs)  =  Just ('*', xs)
star _  =  Nothing

char :: Parser Char
char []= Nothing
char (c:cs) = Just (c, cs)

return :: a -> Parser a
return a cs = Just (a, cs)

--fail ::  Parser a 
--fail cs = Nothing