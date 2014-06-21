{-# LANGUAGE RecordWildCards #-}

module Types
  (
    LispVal(..)

  , LispError(..)
  , ThrowsError
  , throwError

  , LispStack(..)
  ) where

import Control.Monad.Error
import qualified Data.Map as M

import Text.ParserCombinators.Parsec



data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimFun ([LispVal] -> ThrowsError LispVal)
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
  show (PrimFun _) = "<primitive>"
  show (Fun {..}) = "(lambda (" ++ show params {-++ (case vararg of
      Nothing -> ""
      Just arg -> " . " ++ arg
    )-} ++ ") ...)"
  showList xs s = unwords (map show xs) ++ s


data LispError
  = NumArgs Int [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
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
  show (Default s) = "Unclassified error: " ++ s

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError


data LispStack
  = Initial (M.Map String LispVal)
  | Frame (M.Map String LispVal) LispStack
  deriving (Show)