{-# LANGUAGE RecordWildCards #-}

module Types
  (
    LispVal(..)

  , LispError(..)

  , Eval
  , runEval

  , LispStack(..)
  ) where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M
import System.IO

import qualified Text.ParserCombinators.Parsec as Parsec


data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Port Handle
  | PrimFun ([LispVal] -> Eval LispVal)
  | Fun
    { params :: [String]
    --, vararg :: Maybe String
    , body :: [LispVal]
    , closure :: LispStack
    }

instance Show LispVal where
  show (String s) = "\"" ++ s ++ "\""
  show (Atom s) = s
  show (Number n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List xs) = "(" ++ show xs ++ ")"
  show (DottedList xs x) = "(" ++ show xs ++ " . " ++ show x ++ ")"
  show (Port _) = "<IO port>"
  show (PrimFun _) = "<primitive>"
  show (Fun {..}) = "(lambda (" ++ show params {-++ (case vararg of
      Nothing -> ""
      Just arg -> " . " ++ arg
    )-} ++ ") ...)"
  showList xs s = unwords (map show xs) ++ s


data LispError
  = NumArgs Int [LispVal]
  | TypeMismatch String LispVal
  | Parser Parsec.ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | IOError IOError
  | Default String

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found)
    = "Expected " ++ show expected ++ " args; found values " ++ show found
  show (TypeMismatch expected found)
    = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (IOError e) = "IOError: " ++ show e
  show (Default s) = "Unclassified error: " ++ s

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default


type Eval = ErrorT LispError (StateT LispStack IO)

runEval :: Eval a -> LispStack -> IO (Either LispError a, LispStack)
runEval k = runStateT (runErrorT k)


data LispStack
  = Initial (M.Map String LispVal)
  | Frame (M.Map String LispVal) LispStack
  deriving (Show)
