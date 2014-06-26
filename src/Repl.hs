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

evalAndPrint :: LispStack -> String -> IO LispStack
evalAndPrint env s = do
  (result, env') <- runEval (readExpr s >>= eval) env
  case result of
    Left e  -> print e >> return env'
    Right v -> print v >> return env'

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
