-- TCL Subset.
-- Copyright 2011 Henry Strickland.
-- MIT License.

module Main
  where
import Control.Monad.State
import Text.ParserCombinators.Parsec hiding (State)
import qualified System.Environment
import qualified Char(ord)
import qualified Data.Map as M
import qualified Numeric(readDec)
import Debug.Trace(trace)
import qualified Picol501 as Tcl

main :: IO ()
main = do
  args <- System.Environment.getArgs
  let argv = if length args > 0 then tail args else []
  lines <- if length args > 0 then readFile $ head $ args else getContents
  putStrLn $ "INPUT: " ++ (show lines)
  (result, terp) <- runStateT (
      Tcl.evalProcBody ["argv"] lines [Tcl.joinTclList argv]
          ) Tcl.freshTerp
  putStrLn $ "END: " ++ (show terp)
  putStrLn $ "END: " ++ (show result)
