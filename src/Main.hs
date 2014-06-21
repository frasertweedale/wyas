import Control.Monad
import System.Environment

import Repl
import Stack


main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> runRepl initial
    [s] -> runOne initial s
    _   -> putStrLn "args: zero -> repl; one -> eval; _ -> wat"
