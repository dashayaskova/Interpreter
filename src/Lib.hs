module Lib where

import Data.Maybe(fromJust)
import Text.ParserCombinators.Parsec
import MockSample

data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- Змінна
         | Const Value     -- константа
         | Op Exp Bop Exp  -- Операція
                 deriving (Show, Eq)
-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | And | Or | Ba | Bo
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Incr String
          | If Exp Stmt Stmt
          | While Exp Stmt       
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]        
          deriving (Show, Eq)
		  
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = [(String, Value)]

type VarEnv  = [(String,Type)]

----Additional functions for main--------------------------------------------------------------------------
getName :: MockIO m => m String
getName = mGetLine

parseFile :: String ->IO StateW
parseFile file = do 
                 input_info  <- readFile file
                 case parseSPL input_info of
                    Left e  -> error $ show e
                    Right r -> return (evProgram r)
-----------------------------------------------------------------------------------------------------------	


----Main function------------------------------------------------------------------------------------------
getValue ::  StateW -> String -> Value
getValue st i = fromJust $ lookup i st

updValue :: StateW -> String -> Value -> StateW
updValue [] _ _ = []
updValue (s:st) v n 
  | fst s == v = [(fst s, n)] ++ st
  | otherwise = [s] ++ updValue st v n
  
evExp :: StateW -> Exp -> Value
evExp _ (Const constanta) = constanta
evExp state (Var str) = getValue state str
evExp state (Op expression1 o expression2) = calc (evExp state expression1) (evExp state expression2) o

calc :: Value -> Value -> Bop -> Value
calc (I i1) (I i2) Plus = I (i1 + i2)
calc (I i1) (I i2) Minus = I (i1 - i2)
calc (I i1) (I i2) Times = I (i1 * i2)
calc (I i1) (I i2) Div = I (i1 `div` i2)
calc (I i1) (I i2) Gt = B (i1>i2)
calc (I i1) (I i2) Ge = B (i1>=i2)
calc (I i1) (I i2) Lt = B (i1<i2)
calc (I i1) (I i2) Le = B (i1<=i2)
calc (I i1) (I i2) Eql = B (i1==i2)
calc (B b1) (B b2) And = B (b1 && b2)
calc (B b1) (B b2) Or = B (b1 || b2)
calc _ _ _ = error "Not correct expression"

evStmt :: StateW -> Stmt -> StateW 
evStmt state (Assign str e) = updValue state str (evExp state e)
evStmt state (Incr str) = evStmt state (Assign str (Op (Const found) Plus (Const (I 1))))
  where found = getValue state str
evStmt st (If e c1 c2) = if ((evExp st e) == (B True)) then evStmt st c1 else evStmt st c2
evStmt st (While e smt) = if ((evExp st e) == (B True)) then evStmt (evStmt st smt) (While e smt) else st
evStmt st (For smt1 e smt2 smt3) = if ((evExp st1 e) == (B True)) then evStmt st2 (For (Block [] []) e smt2 smt3) else st
  where 
  st1 = evStmt st smt1
  st3 = evStmt st1 smt3
  st2 = evStmt st3 smt2
evStmt st (Block types stmts) = foldl (deleteVar) (foldl (evStmt) new_state stmts) types
  where new_state = (calcStateW types) ++ st
  
  
calcStateW :: [(String,Type)] -> StateW
calcStateW [] = []
calcStateW ((name, t):types) 
 | t == Bt = (name, (B False)) : (calcStateW types)
 | otherwise = (name, (I 0)) : (calcStateW types)


deleteVar :: StateW -> (String,Type) -> StateW
deleteVar [] _ = error "Not correct"
deleteVar (s@(n, _):st)  a@(name, _)
  | name == n = st
  | otherwise = s : (deleteVar st a)
 
 
evProgram :: Program -> StateW
evProgram (Block types stmts) = foldl (evStmt) (calcStateW types) stmts
evProgram _ = error "Not correct"

--------------------------------------------------------------------------------------------------------------


------Function dor syntax parsing-----------------------------------------------------------------------------

{- лексика 
  symbol = ';' | '{' | '}' | '(' | ')' 
  identif=  char {digit | char}
  keyword= "int" | "bool" | "if" | "while" | "for" | "else" | "true" | "false"
  iden   =  identif .... not "int" "bool" "if" "while" "for" "else" "true" "false"
  number = digit { digit }.
  mulOp  = "*" | "/".
  addOp  = "+" | "-".
  relOp  = "<" | "<=" | ">" | ">=" | "==" 
  disOp  = "&" 
  conOp  = "|"
  typev  = "int" | "bool" 
-}

iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","if","while","for","else","True","False"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               } ) 

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- lexem(string str); return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop   
disOp = (oper "&" Ba)

conOp :: Parser Bop   
conOp = (oper "|" Bo)

-- розпізнати всі "порожні" символи в кінці		
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum)) 

typev :: Parser Type 
typev = try(do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt} )

-- Задача 4 -----------------------------------------
identif :: Parser String
identif = do
          x1 <- letter
          x2 <- many (letter <|> digit)
          return (x1:x2)

number :: Parser Int
number  = do
          x1 <- digit
          x2 <- many (digit)
          return (read (x1:x2))
 
addOp :: Parser Bop  
addOp = (oper "+" Plus) <|> (oper "-" Minus)

relOp :: Parser Bop  
relOp = try((oper "<=" Le)
   <|> (oper ">=" Ge) <|> (oper "==" Eql)) <|> (oper "<" Lt) <|> (oper ">" Gt)

{-
factor = '(' expr ')' | number | "true" | "false" | iden
term   = factor { mulOp factor }
relat  = term { addOp term }
conj   = relat [relOp relat] 
disj   = conj { conOp conj}   
expr   = disj { disOp disj} -}
  
factor :: Parser Exp
factor = do { lexem(symbol '('); x <- lexem expr; lexem(symbol ')'); return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {lexem(keyword "true"); return (Const (B True))}
     <|> do {lexem(keyword "false"); return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- Задача 5 -----------------------------------------

additionalOp :: Parser Bop -> Parser (Exp->Exp->Exp)
additionalOp sp = do
    x1 <- lexem sp
    return (\x2 x3 ->(Op x2 x1 x3))

term :: Parser Exp     
term = chainl1 factor (additionalOp mulOp)

relat :: Parser Exp
relat = chainl1 term (additionalOp addOp)

conj :: Parser Exp
conj = chainl1 relat (additionalOp relOp)

disj :: Parser Exp
disj = chainl1 conj (additionalOp conOp)

expr :: Parser Exp
expr = chainl1 disj (additionalOp conOp)

{- оператори
  stmt   = "for" forSt | "while" whileSt | "if" ifSt 
         | iden assSt | blockSt  
  forSt  = '(' stmt ';' expr ';' stmt ')' stmt 
  whileSt= '(' expr ')' stmt 
  ifSt   = '(' expr ')' stmt "else" stmt 
  assSt  = "++" | ":=" expr 
  blockSt= '{' {defin} listSt '}' 
  defin  = type iden ';'
  listSt = stmt {';' stmt}  
  program= stmt eos 
-}   
stmt :: Parser Stmt 
stmt = do {lexem(keyword "for"); lexem forSt}
       <|> do {lexem(keyword "while");lexem whileSt}
       <|> do {lexem(keyword "if"); lexem ifSt}
       <|> do {var <- lexem iden; lexem(assignSt var)}
       <|> blockSt
       <?> "statement"

-- Задача 6 -----------------------------------------
forSt :: Parser Stmt  
forSt   = do
          _ <- lexem(char '(')
          x1 <- lexem(stmt)
          _ <- lexem(char ';')
          x2 <- lexem(expr)
          _ <- lexem(char ';')
          x3 <- lexem(stmt)
          _ <- lexem(char ')')
          x4 <- lexem(stmt)
          return (For x1 x2 x3 x4)

whileSt :: Parser Stmt               
whileSt = do
          _ <- lexem(char '(')
          x1 <- lexem(expr)
          _ <- lexem(char ')')
          x2 <- lexem(stmt)
          return (While x1 x2)
              
ifSt :: Parser Stmt              
ifSt    =  do
          _ <- lexem(char '(')
          x1 <- lexem(expr)
          _ <- lexem(char ')')
          x2 <- lexem(stmt)
          _ <- lexem(string "else")
          x3 <- lexem(stmt)
          return (If x1 x2 x3) 
              
assignSt :: String -> Parser Stmt 
assignSt var = try(do {_ <- lexem(string "++"); return (Incr var)}
    <|> do {_ <- lexem(string ":="); x <- lexem(expr); return (Assign var x)})

defin :: Parser (String, Type)
defin = do
    x1 <- lexem typev
    x2 <- lexem iden
    _ <- lexem (string ";")
    return (x2, x1) 


listSt :: Parser [Stmt]
listSt = do
    x1 <- lexem(stmt)
    x2 <- many (do {_ <- lexem(char ';'); lexem(stmt)})
    return (x1 : x2)

blockSt :: Parser Stmt
blockSt = do
          _ <- lexem(char '{')
          x1 <- many defin
          x2 <- lexem(listSt)
          _ <- lexem(char '}')
          return (Block x1 x2)  
---------------------------------------------	
-- Головні функції
---------------------------------------------				
program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseSPL :: String -> Either ParseError Program
parseSPL s = parse program "" s

					
---------------------------------------------
--- Дані для тестування
--------------------------------------------- 

{- Обчислює значення 12-го числа Фібонначі в змінній out
  {int in; int out; in := 121; 
     {int f0; int f1; int c; f0 := 1; f1 := 1;
      if(in == 0) then out := f0 else 
      if (in == 1) then out := f1 else 
        for (c := 2; c <= in; c++) {
         out := f0 + f1; f0 := f1; f1 := out
        }
	 } 	
  }
-}
fibonacci :: Program
fibonacci = Block [("in",It), ("out",It)]
                  [Assign "in" (Const (I 12)),
                   Block [("f0",It), ("f1",It), ("c",It)]
                          [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                           If (Op (Var "in") Eql (Const (I 0)))
                              (Assign "out" (Var "f0"))
                              (If (Op (Var "in") Eql (Const (I 1)))
                                  (Assign "out" (Var "f1"))
                                  (For (Assign "c" (Const (I 1)))
                                       (Op (Var "c") Lt (Var "in")) (Incr "c")
                                       (Block []
                                             [ Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                             , Assign "f0" (Var "f1")
                                              , Assign "f1" (Var "out")
                                             ])
                                  )
                              )
                          ]
                  ]
				  
power :: String
power =
   "{ int b; int e; int out; b := 6; e := 5; out:= 1;\
   \  for (i:=0; i<e; i++) out := out*b   \
   \}"


powerAST :: Program 
powerAST = Block [("b",It),("e",It),("out",It)]
              [Assign "b" (Const (I 6)), Assign "e" (Const (I 5)), Assign "out" (Const(I 1)),
               Block [("i",It)] 
                     [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                           (Assign "out" (Op (Var "out") Times (Var "b")))
                     ]
              ]

squareRoot :: String
squareRoot =
   "{int a; int b; a := 317; b := 0;\
   \  {bool c; c:=true; while(c) {b++; c:= a >= b*b}};\
   \  b := b-1\
   \ }"

squareRootAST :: Program
squareRootAST = Block [("a",It),("b",It)]
                   [ Assign "a" (Const (I 317)), Assign "b" (Const (I 0)),
                      Block [("c", Bt)] 
                            [Assign "c" (Const (B True)),
                             While (Var "c")
                                 (Block []
                                   [(Incr "b"), 
                                     Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                            ],
                     Assign "b" (Op (Var "b") Minus (Const (I 1)))
                   ]

myExample :: String
myExample = "{int y; int x; bool V; y := 5; x := 6; V := true; if(V) x++ else y++ }"


