-- TCL Subset "Picol501".
-- Copyright 2011 Henry Strickland.
-- MIT License.
--
-- Usage: (result, terp) <-    -- in the IO monad
--            runStateT (Tcl.evalProcBody [] lines []) Tcl.freshTerp

module Picol501
  where
import Control.Monad.State
import Text.ParserCombinators.Parsec hiding (State)
import qualified Char(ord)
import qualified Data.Map as M
import qualified Numeric(readDec,readFloat)
import Debug.Trace(trace)
import qualified IO
import qualified Joy

-- Terp is Tcl inTERPreter state: global commands & a list of stack frames.
data Terp = Terp { cmds :: CmdMap, frames :: [Frame] }
  deriving Show

-- Acting on the interpreter can get&put Terp state, and can do IO.
type Act = StateT Terp IO
-- Most Acts return the Tcl Result type; we call that an Action.
type Action = Act Result

-- The Result of a Tcl Command is either a String (i.e. Right String)
-- or one of several NotOk cases (i.e. Left NotOk).
type Result = Either NotOk String
data NotOk = Err String | Return String | Break | Continue
  deriving Show

-- A stack frame contains a Map of Var names to Var value strings.
type VarMap = M.Map String String
data Frame = Frame VarMap
  deriving Show

-- Commands have names and either a Proc args&body or Bulitin function.
type CmdMap = M.Map String Cmd
data Cmd = Proc [String] String
             | Builtin ([String] -> Action)
instance Show Cmd where
  show (Proc _args _body) = "<Proc>"
  show (Builtin _func) = "<Builtin>"

-- Shortcuts for simple good (i.e. Right) and bad (i.e. Left Err) monadic values.
good = return . Right
bad = return . Left . Err
ok :: Action
ok = good ""              -- ok is the default good action, returning empty string.

say :: String -> Action   -- Debugging Prints.
say s = do {lift $ putStrLn $ "#say# " ++ s; ok}

-- Instead of using the Either Monad, we use ifOk to run another
-- monad if the previous was Right, or produce a bad result
-- previous was Left.
ifOk :: (Show a) => Either a b -> (b -> Act (Either NotOk c)) -> Act (Either NotOk c)
(Left e) `ifOk` _ = do
  say $ "ERROR(ifOk): " ++ show e
  bad $ show e
(Right y) `ifOk` f = f y

-- Just evalling a string does not need pusher or popper, so we use ok & ok.
evalString :: String -> Action
evalString body = eval' body ok ok Left

-- Evalling a proc must push & pop a stack Frame.
evalProcBody :: [String] -> String -> [String] -> Action
evalProcBody params body args = eval' body pushFrame popFrame normalizeReturn
  where
    pushFrame = do
      terp <- get
      put $ terp { frames = Frame M.empty : frames terp }
      z <- putLocalList params args  -- bad if not same length.
      z `ifOk` \_ -> ok  -- but return bad, if bad.
    popFrame = do
      terp <- get
      put $ terp { frames = tail (frames terp) }
      ok
    normalizeReturn (Err s) = Left $ Err s
    normalizeReturn (Break) = Left $ Err "Unepected 'break'"
    normalizeReturn (Continue) = Left $ Err "Unepected 'continue'"
    normalizeReturn (Return x) = Right x

-- Shared eval for both evalString and evalProcBody
eval' :: String -> Action -> Action -> (NotOk -> Result) -> Action
eval' body pusher popper normalizer = do
  (parse parseScript "<INPUT>" body) `ifOk` \sentences -> do
    pusher
    result <- runSentences sentences
    popper
    case result of
      Left x -> return $ normalizer x
      Right x -> good x

----------------------------------------------------------
getVar :: String -> Action
getVar name = do
  let either'word = parse parseWord "<getVar>" name
  either'word `ifOk` \word -> do
    name' <- substWord word
    name' `ifOk` \name'' ->
      if head name'' == ':' then getGlobal (stripColons name'') else getLocal name''

putVar :: String -> String -> Action
putVar name value = do
  let either'word = parse parseWord "<getVar>" name
  either'word `ifOk` \word -> do
    name' <- substWord word
    name' `ifOk` \name'' ->
      if head name'' == ':' then putGlobal (stripColons name'') value else putLocal name'' value

stripColons (':' : s) = stripColons s
stripColons s = s

getGlobal :: String -> Action
getGlobal name = do
  terp <- get
  let (Frame vars) = last $ frames terp  -- there must be a last frame.
  case M.lookup name vars of
    Nothing -> bad $ "ERROR: not found in getGlobal " ++ name
    Just z -> good z

getLocal :: String -> Action
getLocal name = do
  terp <- get
  let (Frame vars) : _ = frames terp  -- there must be a head frame.
  case M.lookup name vars of
    Nothing -> bad $ "ERROR: not found in getLocal " ++ name
    Just z -> good z

putGlobal :: String -> String -> Action
putGlobal name value = do
  terp <- get
  let fs = frames terp
  let Frame last_vars = last fs
  put $ terp { frames = init fs ++ [Frame (M.insert name value last_vars)] }
  good $ value

putLocal :: String -> String -> Action
putLocal name value = do
  terp <- get
  let fs = frames terp
  let Frame vars = head fs
  put $ terp { frames = Frame (M.insert name value vars) : tail fs }
  good $ value

putLocalList :: [String] -> [String] -> Action
putLocalList [] [] = ok
putLocalList [] _ = bad "Too Many Args"
putLocalList _ [] = bad "Too FeW Args"
putLocalList (name:names) (value:values) = do
  putLocal name value         -- put the head
  putLocalList names values   -- recurse the tail

----------------------------------------------------------
-- Fresh instance of the interpereter state and bulitin cmds.
freshTerp = Terp { cmds = freshCmds, frames = [] }
  where
    freshCmds :: CmdMap
    freshCmds =
      M.insert "k" (Builtin builtin_k) $
      M.insert "set" (Builtin builtin_set) $
      M.insert "incr" (Builtin builtin_incr) $
      M.insert "append" (Builtin builtin_append) $
      M.insert "lappend" (Builtin builtin_lappend) $
      M.insert "proc" (Builtin builtin_proc) $
      M.insert "assert" (Builtin builtin_assert) $
      M.insert "catch" (Builtin builtin_catch) $
      M.insert "+"  (Builtin (foldop (+) 0)) $
      M.insert "*"  (Builtin (foldop (*) 1)) $
      M.insert "-"  (Builtin (binop (-))) $
      M.insert "/"  (Builtin builtin_intdiv) $
      M.insert "==" (Builtin (numrelop (==))) $
      M.insert "!=" (Builtin (numrelop (/=))) $
      M.insert "<"  (Builtin (numrelop (< ))) $
      M.insert "<=" (Builtin (numrelop (<=))) $
      M.insert ">"  (Builtin (numrelop (> ))) $
      M.insert ">=" (Builtin (numrelop (>=))) $
      M.insert "eq" (Builtin (relop (==))) $
      M.insert "ne" (Builtin (relop (/=))) $
      M.insert "lt"  (Builtin (relop (< ))) $
      M.insert "le" (Builtin (relop (<=))) $
      M.insert "gt"  (Builtin (relop (> ))) $
      M.insert "ge" (Builtin (relop (>=))) $

      M.insert "break" (Builtin $ \[] -> return $ Left $ Break) $
      M.insert "continue" (Builtin $ \[] -> return $ Left $ Continue) $
      M.insert "error" (Builtin $ \[e] -> return $ Left $ Err e) $
      M.insert "return" (Builtin builtin_return) $
      M.insert "foreach" (Builtin builtin_foreach) $
      M.insert "if" (Builtin builtin_if) $
      M.insert "while" (Builtin builtin_while) $
      M.insert "for" (Builtin builtin_for) $
      M.insert "lindex" (Builtin builtin_lindex) $
      M.insert "list" (Builtin builtin_list) $
      M.insert "split" (Builtin builtin_split) $
      M.insert "puts" (Builtin builtin_puts) $
      M.insert "gets" (Builtin builtin_gets) $
      M.insert "joy" (Builtin builtin_joy) $
      M.empty

builtin_intdiv :: [String] -> Action
builtin_intdiv [x, y]  |  atoi y == 0  =  bad "Division By Zero"
builtin_intdiv [x, y] = good $ show $ (atoi x) `div` (atoi y)

-- 0 is false, other numbers are true.  N.B. in atoi, Parse errors are 0.
truthOfTclString s = if atoi s == 0 then False else True

-- for "+" and "*" commands, which will fold.
foldop op init args = good $ show $ foldr op init $ map atoi args

-- for other numeric binary operations.
binop op [a, b] = good $ show $ atoi a `op` atoi b

-- for relational operators on strings.  e.g. "eq", "lt"
relop op [a, b] = good $ if a `op` b then "1" else "0"

-- for relational operators on numbers.  e.g. "==", "<"
numrelop op [a, b] = good $ if (atoi a) `op` (atoi b) then "1" else "0"

-- produce speical "Left Return String" result.
builtin_joy [s] = good $ Joy.evalJoy s

builtin_return [] = return $ Left $ Return ""
builtin_return [x] = return $ Left $ Return x

builtin_foreach :: [String] -> Action
builtin_foreach [var, list, body] = do
  either'vec <- splitTclList list
  either'vec `ifOk` \vec -> runLoop vec
    where
      runLoop [] = ok
      runLoop (item : items) = do
          putLocal var item
          r <- evalString body
          case r of
            Left Break -> ok
            Left Continue -> recurse'
            Right _ -> recurse'
            e -> return e
          where recurse' = runLoop items

builtin_for :: [String] -> Action
builtin_for [init, cond, inc, body] = do
  r0 <- evalString init
  r0 `ifOk` \_ -> do
    r1 <- evalString cond
    r1 `ifOk` \s1 ->
      if truthOfTclString s1
        then do
          r2 <- evalString body
          case r2 of
            Left Break -> ok
            Left Continue -> recurse'
            Right _ -> do
              r3 <- evalString inc
              r3 `ifOk` \_ ->
                recurse'
            otherwise -> return otherwise
        else ok
      where recurse' = builtin_for ["", cond, inc, body]

builtin_while :: [String] -> Action
builtin_while [cond, body] = do
  r1 <- evalString cond
  r1 `ifOk` \s1 ->
    if truthOfTclString s1
      then do
        r2 <- evalString body
        case r2 of
          Left Break -> ok
          Left Continue -> recurse'
          Right _ -> recurse'
          e -> return e
      else ok
    where recurse' = builtin_while [cond, body]


builtin_puts :: [String] -> Action
builtin_puts [str] = do
  lift $ putStrLn str
  ok

builtin_gets :: [String] -> Action
builtin_gets ["stdin", bufVarName] = do
  eof <- lift $ IO.hIsEOF IO.stdin
  if eof
    then do
      putVar bufVarName ""
      good "-1"
    else do
      s <- lift $ IO.getLine
      putVar bufVarName s
      good $ show $ length s

builtin_lindex :: [String] -> Action
builtin_lindex [list, i] = do
  either'vec <- splitTclList list
  either'vec `ifOk` \vec -> good $ vec !! atoi i


builtin_list :: [String] -> Action
builtin_list args = good $ joinTclList args

builtin_split :: [String] -> Action
builtin_split [str] = good $ joinTclList $ splitOnWhite str
builtin_split [str, (c:[])] = good $ joinTclList $ splitOnChar c str

-- k returns its first argument.  If no args, it returns empty.
builtin_k [] = ok
builtin_k (first:_) = good first

builtin_assert [cond] = builtin_assert [cond, "untitled"]
builtin_assert [cond, msg] = if truthOfTclString cond
  then ok
  else bad $ "Assertion Failed: " ++ msg

builtin_catch [body] = do
  result <- evalString body
  case result of
    Left e -> good "1"
    Right s -> good "0"

builtin_if [cond, onTrue, onFalse] = if truthOfTclString cond
  then evalString onTrue
  else evalString onFalse
builtin_if [cond, onTrue] = builtin_if [cond, onTrue, ""]
builtin_if [cond, onTrue, kwElse, onFalse] = if kwElse == "else"
  then builtin_if [cond, onTrue, onFalse]
  else bad "expected {else} as third arg to if"
builtin_if args = bad $ "bad args: " ++ show args

builtin_set [name, value] = putVar name value
builtin_set [name] = getVar name

builtin_incr [name] = builtin_incr [name, "1"]
builtin_incr [name, value] = do
  r <- getVar name
  r `ifOk` \s -> putVar name (show (atoi s + atoi value))

builtin_append [name, value] = do
  r <- getVar name
  r `ifOk` \s -> putVar name (s ++ value)

builtin_lappend [name, value] = do
  r <- getVar name
  r `ifOk` \s -> putVar name (s ++ " " ++ escapeTclListItem value)

builtin_proc [name, params, body] = do
  x <- splitTclList params
  case x of
    Left e -> return $ Left e
    Right paramList -> do
      terp <- get
      put $ terp { cmds = M.insert name (Proc paramList body) $ cmds terp }
      ok

splitTclList :: String -> Act (Either NotOk [String])
splitTclList s = (parse parseTclList "<LIST>" s) `ifOk` \z -> good z

joinTclList :: [String] -> String
joinTclList [] = ""
joinTclList (s:ss) = "{" ++ escapeTclListItem s ++ "} " ++ joinTclList ss

escapeTclListItem :: String -> String
escapeTclListItem [] = ""
escapeTclListItem ('{':cs) = '\\' : '{' : escapeTclListItem cs
escapeTclListItem ('}':cs) = '\\' : '}' : escapeTclListItem cs
escapeTclListItem ('\\':cs) = '\\' : '\\' : escapeTclListItem cs
escapeTclListItem (c:cs) = c : escapeTclListItem cs

------------ Running Sentences. -------------------------
-- Nomenclature: Sentence is a list of Words, to be Tcl-substituted into strings, and then
-- the strings are applied as a Tcl command.

-- Run a list of commands, stopping if any is NotOk.
runSentences :: [Sentence] -> Action
runSentences [] = ok          -- case of empty sentences.
runSentences (sentence:sentences) = do
  terp <- get
  --say ("<<<<<<<<< " ++ show terp)
  say ("<<<<<< " ++ show sentence)
  result <- runSentence sentence
  say (">>>>>> " ++ show result)
  terp' <- get
  --say (">>>>>>>>> " ++ show terp')
  case result of
    Left e -> return $ Left e  -- stop when one is NotOk
    Right _ -> finish sentences  -- continue executing sentences when Right.
      where
        finish [] = return result            -- usual stop to recursion.
        finish sentences = runSentences sentences  -- run the rest.

-- Run one command, first doing needed substitutions on each word.
runSentence :: Sentence -> Action
runSentence (Sentence words) = do
  say ("<<< " ++ show words)
  terp <- get
  rs <- substWords words
  say ("rs: " ++ show rs)
  rs `ifOk` \(rstr:astrs) -> do
    case (M.lookup rstr $ cmds terp) of
      Nothing -> do
        say (">>> Nothing: " ++ rstr)
        bad $ "Cannot find cmd: " ++ rstr
      Just cmd -> do
        say (">>> Guts: " ++ show cmd)
        z <- runCmd cmd astrs
        say (">>> z: " ++ show z)
        return z

-- Running a command that is a Proc
runCmd (Proc params body) args = do
  say $ show $ (params, body, args)
  evalProcBody params body args

-- Running a command that is a Builtin
runCmd (Builtin func) args = do
  say $ show $ ("builtin func args=", args)
  func args

-----------------------------------------------
-- Scripts parse into Sentences of Words of Fragments.
data Fragment = BareFragment String  -- Unit of substitution.
              | VarFragment String
              | CmdFragment String
  deriving Show
data Word = Word [Fragment]          -- Individual args to commands.
  deriving Show
data Sentence = Sentence [Word]      -- Individual commands.
  deriving Show

------------------------------------------------
substWords :: [Word] -> Act (Either NotOk [String])
substWords [] = good $ []
substWords (w : ws) = do
  x <- substWord w
  x `ifOk` \s -> do
      y <- substWords ws
      case y of
        Left e -> do
          return $ Left e
        Right ss -> do
          good $ s : ss

substWord :: Word -> Action
substWord w = substWord' [] w
  where
    substWord' :: [String] -> Word -> Action
    substWord' stringsDone (Word []) = good $ concat $ reverse stringsDone
    substWord' stringsDone (Word (frag:frags)) = do
      a <- substFragment' frag
      a `ifOk` \s -> substWord' (s:stringsDone) (Word frags)
    substFragment' :: Fragment -> Action
    substFragment' (BareFragment s) = good s
    substFragment' (VarFragment s) = getVar s
    substFragment' (CmdFragment s) = evalString s

-----  Parsers!   --------------------------------------------
parseScript :: Parser [Sentence]
parseScript = do
  skipToNextCommand
  (eof >> return []) <|> parseSentences

parseSentences :: Parser [Sentence]
parseSentences = do
  c <- parseSentence
  cs <- parseScript
  return $ c : cs

skipToNextCommand :: Parser ()
skipToNextCommand = skip $ many $ (blank <|> comment <|> endCommand)

parseSentence :: Parser Sentence
parseSentence = do
  words <- many1 $ parseWord
  return $ Sentence words

parseWord :: Parser Word
parseWord = do
  frags <- parseDoubleQuotedFragments <|> many1 parseFragment
  many $ oneOf $ blankChars
  return $ Word frags

parseDoubleQuotedFragments :: Parser [Fragment]
parseDoubleQuotedFragments = do
  char '\"'
  frags <- parseInsideDoubleQuotes <|> return []
  char '\"'
  return (trace ("### dqfrags=" ++ show frags) frags)

parseInsideDoubleQuotes :: Parser [Fragment]
parseInsideDoubleQuotes = do
  frag <- parseCmdFragment <|> parseVarFragment <|> parseBareFragment <|> parseBlankFragment
  frags <- parseInsideDoubleQuotes <|> return []
  return $ frag : frags

parseFragment :: Parser Fragment
parseFragment = parseCmdFragment <|> parseVarFragment <|> parseBareFragment

parseBareFragment = do
  z <- parseCurlyQuoted <|> bares
  return $ BareFragment z

parseVarFragment = do
  z <- varDeref
  return $ VarFragment z

parseCmdFragment = do
  z <- parseSquareBracketed
  return $ CmdFragment z

parseBlankFragment = do
  z <- many1 $ oneOf $ blankChars
  return $ BareFragment z

parseDoubleQuoted :: Parser String
parseDoubleQuoted = do
  char '\"'
  z <- many $     parseNestedSquareBracketed
              <|> parseEscapedChar
              <|> (many1 $ noneOf "{}[]\\\"")
  char '\"'
  return $ concat (trace ("### @parseDoubleQuoted@ " ++ show z) z)

parseNestedDoubleQuoted = do
  s <- parseDoubleQuoted
  return $ "\"" ++ s ++ "\""

parseCurlyQuoted :: Parser String
parseCurlyQuoted = do
  char '{'
  z <- many $     parseNestedCurlyQuoted
              <|> parseEscapedChar
              <|> (many1 $ noneOf "{}\\")
  char '}'
  return $ concat (trace ("### @parseCurlyQuoted@ " ++ show z) z)

parseNestedCurlyQuoted = do
  s <- parseCurlyQuoted
  return $ "{" ++ s ++ "}"

parseSquareBracketed :: Parser String
parseSquareBracketed = do
  char '['
  z <-  many $    parseNestedCurlyQuoted
              <|> parseNestedSquareBracketed
              <|> parseNestedDoubleQuoted
              <|> parseEscapedChar
              <|> (many1 $ noneOf "[]{}\\\"")
  char ']'
  return $ concat (trace ("### @parseSquareBracketed@ " ++ show z) z)

parseNestedSquareBracketed = do
  s <- parseSquareBracketed
  return $ "[" ++ s ++ "]"

parseTclList :: Parser [String]
parseTclList = do
  many white
  (eof >> return []) <|> parseItems
    where
        parseItems = do
          s <-     do { t <- parseNestedCurlyQuoted; return $ init $ tail t }
               <|> parseEscapedChar
               <|> (many1 $ noneOf $ whiteChars)
          ss <- parseTclList
          return $ s : ss

parseEscapedChar :: Parser String
parseEscapedChar = do
  char '\\'
  c <- noneOf "\n"          -- TODO: handle \\ before \n
  return $ '\\' : c : ""

alphanumChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
specialChars = "{}[];\"$\\"
blankChars = "\t\v "
endCommandChars = "\n\r;"
whiteChars = "\t\n\r\v "

bares :: Parser String
bares = many1 $ noneOf $ specialChars ++ blankChars ++ endCommandChars

white = skip $ oneOf $ whiteChars
blank = skip $ oneOf $ blankChars
endCommand = skip $ oneOf $ endCommandChars

comment = do
  char '#'
  many $ noneOf "\n"
  char '\n'
  return ()

subscript :: Parser String
subscript = do
  char '('
  s <- many $ noneOf $ "()[]{}\\"  -- TODO: make this better.
  char ')'
  return $ "(" ++ s ++ ")"

varDeref :: Parser String
varDeref = do
  char '$'
  name <- many1 $ oneOf $ alphanumChars ++ ":"
  sub <- subscript <|> (return "")
  return $ name ++ sub

skip :: Parser a -> Parser ()
skip p = p >> return ()

-------------------------- Utilities ------------------

atoi :: String -> Int
atoi ('-':s)  = -(atoi s)  -- Numeric.readDec doesn't handle negatives.
atoi s = case Numeric.readDec s of
  [(a, _)] -> a  -- Like atoi() in C, ignore remainder of string.
  _ -> 0         -- Like atoi() in C, parse error is 0.


splitOnChar :: Char -> String -> [String]
splitOnChar c s = z
  where
    Right z = parse (split' c) "<split>" s
    split' :: Char -> Parser [String]
    split' c = do
      hd <- (do
        t <- (many1 $ noneOf $ c:"")
        return $ Just t
          ) <|> (eof >> return Nothing)
      case hd of
        Nothing -> return []
        Just t -> do
          b <- (char c >> return True) <|> return False
          if b
            then do
              ts <- split' c
              return $ t : ts
            else return [t]

splitOnWhite :: String -> [String]
splitOnWhite s = z
  where
    Right z = parse split' "<split>" s
    split' :: Parser [String]
    split' = do
      skip $ many $ oneOf $ whiteChars
      hd <- (many1 $ noneOf $ whiteChars) <|> (eof >> return "")
      if hd == ""
        then return []
        else do
           ts <- split'
           return $ hd : ts

