-- Tests for TCL Subset.
-- Copyright 2011 Henry Strickland.
-- MIT License.

module Main
  where
import Control.Monad.Trans.Either
import Control.Monad.State
import Text.ParserCombinators.Parsec hiding (State)
import qualified Char(ord)
import qualified Data.Map as M
import qualified Numeric(readDec)
import qualified Debug.Trace(trace)
import qualified Picol501 as Tcl
import qualified System.Exit

main :: IO ()
main = do
  runTests
  putStrLn "ALL OK."

assert :: String -> Bool -> IO ()
assert name True = do
  putStrLn $ "#good# " ++ name
  return ()
assert name False = do
  error $ "FAILED: " ++ name
  return ()

goodScript expected script = do
  (result, terp) <- runStateT (Tcl.evalProcBody [] script []) Tcl.freshTerp
  case result of
    Left x -> error $ "FAILED: " ++ script ++ " ==>> " ++ show x
    Right x -> if x == expected
      then putStrLn $ "good: " ++ script ++ " ==>> " ++ show x
      else error $ "FAILED: " ++ script ++ " ==>> " ++ show x ++ " <<== Expected " ++ show expected

runTests = do
  assert "foo" $ 1 == 1
  assert "bar" $ 1 == 1
  goodScript "3" "+ 1 2"
  goodScript "0" "catch {+ 0}"
  goodScript "0" "catch {+ 1 2 3}"
  goodScript "1" "catch {foo 1 2 3}"
  goodScript "15" "+ 1 2 [+ 3 4 5]"
  goodScript "42" "if [== 3 3] {+ 42} else {+ 13}"
  goodScript "42" "if [< 2 3] {+ 42} else {+ 13}"
  goodScript "42" "if [> 4 3] {+ 42} else {+ 13}"
  goodScript "42" "proc plus {a b} { + $a $b } ; plus 38 4"
  goodScript "42" "proc plus \"a b\" { + $a $b } ; plus 38 4"

  goodScript "0" "proc zero {a} {if [< $a 1] {+ $a} else {zero [+ -1 $a]}} ; zero 3"

  goodScript "0" "proc zero {a} {if [< $a 1] {return $a} else {zero [+ -1 $a]}} ; zero 3"
  goodScript "1" "proc zero {a} {if [< $a 1] {error BAD} else {zero [+ -1 $a]}} ; catch {zero 3}"
  goodScript "1" "proc zero {a} {if [< $a 1] {break} else {zero [+ -1 $a]}} ; catch {zero 3}"
  goodScript "1" "proc zero {a} {if [< $a 1] {continue} else {zero [+ -1 $a]}} ; catch {zero 3}"

  goodScript "21" "proc tri {n} { if [< $n 1] { + 0 } else { + $n [tri [+ $n -1]] } }; tri 6"
  goodScript "28" "proc tri {n} { set z 0; while {> $n 0} {set z [+ $z $n]; set n [+ $n -1]}; k $z}; tri 7"
  goodScript "28" "proc tri {n} { set z 0; while {k 1} {set z [+ $z $n]; set n [+ $n -1]; if [< $n 1] break}; k $z}; tri 7"
  goodScript "28" "proc tri {n} { set z 0; while {k 1} {set z [+ $z $n]; set n [+ $n -1]; if [< $n 1] break else continue}; k $z}; tri 7"

  goodScript "42" "set a(12345) 40 ; set a(foo) 2 ; + $a(12345) $a(foo)"
  goodScript "42" "set ::x 23; proc f {} {set ::x 42}; f; set ::x"
  goodScript "15" "set z 0; foreach i {1 2 3 4 5} {set z [+ $z $i]}; set z"
  goodScript "15" "set z 0; foreach i {1 2 3 4 5} {incr z $i}; set z"
  goodScript "012345" "set z 0; foreach i {1 2 3 4 5} {append z $i}; set z"

  -- joy
  goodScript "5 5 4 3 2 1" "joy {1 2 3 4 5 dup}"
  goodScript "4 3 2 1" "joy {1 2 3 4 5 zap}"
  goodScript "4 5 3 2 1" "joy {1 2 3 4 5 swap}"
  goodScript "{{4 44 444} 5 55 555} 3 2 1" "joy {1 2 3 {4 44 444} {5 55 555} cons}"
  goodScript "{4 44 444 5 55 555} 3 2 1" "joy {1 2 3 {4 44 444} {5 55 555} cat}"
  goodScript "5 5 5 4 3 2 1" "joy {1 2 3 4 5 {dup dup} i}"
  goodScript "5 4 4 4 3 2 1" "joy {1 2 3 4 5 {dup dup} dip}"
  goodScript "5 4 4 4 3 2 1" "joy {{dup} ^DUP 5 ^FIVE 4 ^FOUR 1 2 3 FOUR FIVE {DUP i dup} dip}"
