import Control.Monad
import System.Environment

import Repl
import Lib


main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> runRepl lib
    [s] -> runOne lib s
    _   -> putStrLn "args: zero -> repl; one -> eval; _ -> wat"
