module Repl
  (
    runOne
  , runRepl
  ) where

import Control.Monad
import System.IO

import Eval
import Stack

evalAndPrint :: LispStack -> String -> IO LispStack
evalAndPrint env s =
  case readExpr s >>= eval env of
    Left e          -> print e >> return env
    Right (v, env') -> print v >> return env'

runOne :: LispStack -> String -> IO ()
runOne env expr = void $ evalAndPrint env expr

runRepl :: LispStack -> IO ()
runRepl env = do
  expr <- readPrompt ">>> "
  unless (expr == "quit") (evalAndPrint env expr >>= runRepl)

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt s = flushStr s >> getLine
