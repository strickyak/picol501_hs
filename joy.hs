-- JOY Subset.
-- Copyright 2011 Henry Strickland.
-- MIT License.
--
-- Usage: (result, terp) <-    -- in the IO monad
--            runStateT (Tcl.evalProcBody [] lines []) Tcl.freshTerp
--
-- Changes from standard Joy:
--   Use { } for quoting, instead of [ ].  It's easier in TCL that way.
--   Use ^A for lambda A.   It's easier in TCL than using \ for lambda.
--   Lambda is implemented by popping from stack, and substituting in the rest of the list.

module Joy
  where
import Control.Monad.State
import Text.ParserCombinators.Parsec hiding (State)
import qualified Char(ord)
import qualified Data.Map as M
import qualified Numeric(readDec,readFloat)
import Debug.Trace(trace)
import qualified IO

data JVal = JSym String
         | JNum Int
	 | JStr String
	 | JLambda String
	 | JSet Int
	 | JChar Char
	 | JList [JVal]
	 | JBad String
  deriving Show

evalJoy :: String -> String
evalJoy s =
  case parse parseJoy "<JOY>" s of
    Left s -> "Parse ERROR: " ++ show s
    Right js -> showJList (eval' js [])

eval' :: [JVal] -> [JVal] -> [JVal]
eval' [] stack = stack
eval' (JLambda name:js) (s1:stack) = eval' (subst' name s1 js) stack
eval' (j:js) stack = eval' js (eval'1'trace j stack)

subst' name r [] = []
subst' name r (JSym s : js) =
  (if name == s then r else JSym s) : subst' name r js
subst' name r (JList inner : js) =
  JList (subst' name r inner) : subst' name r js
subst' name r (j : js) = j : subst' name r js

eval'1'trace :: JVal -> [JVal] -> [JVal]
eval'1'trace j stack = trace (
  "<<   " ++ showJList  stack ++ "   >>   " ++ showJoy j ++ "   --->   " ++ showJList  result
  ) result
    where result = eval'1 j stack

eval'1 :: JVal -> [JVal] -> [JVal]

eval'1 (JSym "dup") (s1:stack) = s1 : s1 : stack
eval'1 (JSym "swap") (s1:s2:stack) = s2 : s1 : stack
eval'1 (JSym "zap") (s1:stack) = stack

eval'1 (JSym "unit") (s1:stack) = JList [s1] : stack
eval'1 (JSym "cons") ((JList x1):s2:stack) = JList (s2 : x1) : stack
eval'1 (JSym "cat") ((JList x1):(JList x2):stack) = JList (x2 ++ x1) : stack

eval'1 (JSym "i") ((JList x1):stack) = eval' x1 stack
eval'1 (JSym "dip") ((JList x1):s2:stack) = s2 : eval' x1 stack

eval'1 (JSym s) (stack) = JSym (s ++ "_") : stack
eval'1 j (stack) = j : stack


showJoy :: JVal -> String
showJoy x = case x of
  JSym s -> s
  JNum n -> show n
  JStr s -> "\"" ++ s ++ "\""
  JChar c -> '\'' : c : '\'' : []
  JList vs -> "{" ++ showJList vs ++ "}"

showJList :: [JVal] -> String
showJList [] = ""
showJList [j] = showJoy j
showJList (j:js) = showJoy j ++ " " ++ showJList js

parseJoy :: Parser [JVal]
parseJoy = do
  skipWhite
  ts <- terms
  eof
  return ts

terms :: Parser [JVal]
terms = do
  ts <- (char '}' >> return []) <|> (eof >> return []) <|> terms1
  skipWhite
  return ts

terms1 :: Parser [JVal]
terms1 = do
  t <- term
  ts <- terms
  return $ t : ts

term :: Parser JVal
term = do
  t <- alfaSym <|> symSym <|> numLit <|> strLit <|> jlist <|> lambda
  skipWhite
  return t

jlist :: Parser JVal
jlist = do
  char '{' >> skipWhite
  ts <- terms
  -- char '}' >> skipWhite
  return $ JList ts

lambda :: Parser JVal
lambda = do
  oneOf "^\\"  -- support either ^ or \ for lambda
  JSym name <- alfaSym
  return $ JLambda name

alfaSym :: Parser JVal
alfaSym = do
  c <- oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
  cs <- many $ oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890"
  skipWhite
  return $ JSym $ c : cs

symSym :: Parser JVal
symSym = do
  s <- many1 $ oneOf "+-*/%&|<>=!@$~"
  skipWhite
  return $ JSym s

strLit :: Parser JVal
strLit = do
  char '"'
  s <- many1 $ noneOf "\""
  char '"'
  return $ JStr s

numLit :: Parser JVal
numLit = do
  s <- many1 $ oneOf "1234567890"
  return $ JNum $ atoi s

skipWhite = do
  many $ (oneOf " \t\v\n\r" >> return ()) <|> comment
  return ()
comment = do
  char '#'
  many $ noneOf "\n"
  char '\n'
  return ()

atoi :: String -> Int
atoi ('-':s)  = -(atoi s)  -- Numeric.readDec doesn't handle negatives.
atoi s = case Numeric.readDec s of
  [(a, _)] -> a  -- Like atoi() in C, ignore remainder of string.
  _ -> 0         -- Like atoi() in C, parse error is 0.


