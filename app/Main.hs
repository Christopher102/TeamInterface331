{-
 -  Main.hs
 -
 -  Reference implementation of the toy languages from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides routines for executables based on HOPL languages.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import Control.Exception (ErrorCall, catch)
import Control.Monad (unless, void)
import Control.Monad.Trans (liftIO)
import qualified HOPL.CALL_BY_NEED.Interp as CALL_BY_NEED (interp)
import qualified HOPL.CALL_BY_REFERENCE.Interp as CALL_BY_REFERENCE (interp)
import qualified HOPL.CHECKED.Interp as CHECKED (checkAndInterp)
import qualified HOPL.CHECKED_STATEMENT.Interp as CHECKED_STATEMENT (checkAndInterp)
import qualified HOPL.EXPLICIT_REFS.Interp as EXPLICIT_REFS (interp)
import qualified HOPL.IMPLICIT_REFS.Interp as IMPLICIT_REFS (interp)
import qualified HOPL.INFERRED.Interp as INFERRED (checkAndInterp)
import qualified HOPL.LET.Interp as LET (interp)
import qualified HOPL.LETREC.Interp as LETREC (interp)
import qualified HOPL.MUTABLE_PAIRS.Interp as MUTABLE_PAIRS (interp)
import qualified HOPL.PROC.Interp as PROC (interp)
import qualified HOPL.SIMPLE_STATEMENT.Interp as SIMPLE_STATEMENT (interp)
import qualified HOPL.STATPY.Interp as STATPY (checkAndInterp)
import HOPL.Types (Interpreter, Source)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

repl :: IO ()
repl = do
  args <- getArgs
  let lang = if null args then "LET" else head args
  runInputT defaultSettings (loop lang)
  where
    loop lang = do
      minput <- getInputLine (lang ++ "> ")
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input ->
          unless (input == ":q") $
            liftIO
              ( case lang of
                  "LET" -> doInterp LET.interp input
                  "PROC" -> doInterp PROC.interp input
                  "LETREC" -> doInterp LETREC.interp input
                  "EXPLICIT_REFS" -> doInterp EXPLICIT_REFS.interp input
                  "IMPLICIT_REFS" -> doInterp IMPLICIT_REFS.interp input
                  "MUTABLE_PAIRS" -> doInterp MUTABLE_PAIRS.interp input
                  "CALL_BY_REFERENCE" -> doInterp CALL_BY_REFERENCE.interp input
                  "CALL_BY_NEED" -> doInterp CALL_BY_NEED.interp input
                  "SIMPLE_STATEMENT" -> doInterp' SIMPLE_STATEMENT.interp input
                  "CHECKED" -> doInterp CHECKED.checkAndInterp input
                  "INFERRED" -> doInterp INFERRED.checkAndInterp input
                  "CHECKED_STATEMENT" -> doInterp' CHECKED_STATEMENT.checkAndInterp input
                  "STATPY" -> doInterp STATPY.checkAndInterp input
              )
              >> loop lang

doInterp :: Show a => Interpreter a -> Source -> IO ()
doInterp interp input =
  case interp input of
    Left err -> print err
    Right val -> print val
    `catch` (\e -> hPrint stderr (e :: ErrorCall))

doInterp' :: Interpreter (IO a) -> Source -> IO ()
doInterp' interp input =
  case interp input of
    Left err -> print err
    Right val -> void val
    `catch` (\e -> hPrint stderr (e :: ErrorCall))

run :: IO ()
run = do
  args <- getArgs
  if null args
    then putStrLn "hopl3-run: Missing source file name"
    else do
      prog <- readFile $ head args
      case STATPY.interp prog of
        Left err -> print err
        Right val -> print val
        `catch` (\e -> hPrint stderr (e :: ErrorCall))
      return ()
