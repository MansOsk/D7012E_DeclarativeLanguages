import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App oper a) = oper ++ "(" ++ unparse a ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" x) env = sin (eval x env)
eval (App "cos" x) env = cos (eval x env)
eval (App "log" x) env = log (eval x env)
eval (App "exp" x) env = exp (eval x env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App "sin" x) = Op "*" (diff v x) (App "cos" x)  -- sin' = kcoskx
diff v (App "cos" x) = Op "*" (diff v x) (Op "*" (Const (-1)) (App "sin" x)) -- cos' = -ksinkx
diff v (App "log" x) = Op "*" (diff v x) (Op "/" (Const 1) x) -- ln(kx)' = k(1/kx)
diff v (App "exp" x) = Op "*" (diff v x) (App "exp" x)  -- e^(kx)' = ke^(kx)
diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (App oper x) = App oper (simplify x)
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> 
        if left==right 
          then Const 0 
          else Op "-" le re
      (op,le,re)      -> Op op le re

mkfun :: (EXPR,EXPR) -> (Float -> Float)  mkfun (parse "x*x+2", Var "x") 3.0 
mkfun (body, Var v) x = eval body [(v, x)]

findzero :: String -> String -> Float -> Float
findzero s1 s2 = raphson
  where
    f = mkfun (parse s2, Var s1)
    f' = mkfun (diff (Var s1) (parse s2), Var s1)
    raphson x0
      | abs (x1 - x0) > 0.0001 = raphson x1     -- xn-3 xn-2  .. xn-2 xn-1 .. xn-1 xn 
      | otherwise = x0
      where
        x1 = x0 - f x0 / f' x0

main = do
  print $ unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))
  print $ unparse (simplify (diff (Var "y") (parse "cos(y)*sin(y)+sin(y)")))

  print $ mkfun (parse "x*x+2", Var "x") 3.0 
  print $ mkfun (parse "x+2", Var "x") 3.0
  print $ mkfun (parse "exp(x)+2", Var "x") 3.0 

  print $ findzero "x" "x*x*x+x-1" 1.0
  print $ findzero "y" "cos(y)*sin(y)" 2.0 