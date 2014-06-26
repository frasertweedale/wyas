import Control.Monad
import System.Environment
import qualified Data.Map as M

import Lib
import Repl
import Types


main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> runRepl lib
    (s:args') ->
      runOne s $ Frame (M.singleton "args" (List $ map String args')) lib
