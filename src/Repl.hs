module Repl
  (
    runOne
  , runRepl
  ) where

import Control.Monad
import System.IO

import Eval
import Stack
import Types

evalAndPrint :: Eval LispVal -> LispStack -> IO LispStack
evalAndPrint k env = do
  (result, env') <- runEval k env
  case result of
    Left e  -> hPrint stderr e >> return env'
    Right v -> print v >> return env'

runOne :: String -> LispStack -> IO ()
runOne s = void . evalAndPrint (eval (List [Atom "load", String s]))

runRepl :: LispStack -> IO ()
runRepl env = do
  s <- readPrompt ">>> "
  unless (s == "quit") (evalAndPrint (readExpr s >>= eval) env >>= runRepl)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt s = flushStr s >> getLine
