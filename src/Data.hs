module Data
  (
    LispVal(..)
  ) where


data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

instance Show LispVal where
  show (String s) = "\"" ++ s ++ "\""
  show (Atom s) = s
  show (Number n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List xs) = "(" ++ show xs ++ ")"
  show (DottedList xs x) = "(" ++ show xs ++ " . " ++ show x ++ ")"
  showList xs s = unwords (map show xs) ++ s
