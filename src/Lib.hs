module Lib
  (
    lib
  ) where

import Control.Arrow
import Control.Monad.Error
import qualified Data.Map as M
import System.IO

import Eval
import Types


lib :: LispStack
lib = Initial $ M.fromList $ map (second PrimFun)
  [
    ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-file", closePort)
  , ("close-output-file", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]
  where
    numBoolBinop  = boolBinop unpackNum
    strBoolBinop  = boolBinop unpackStr
    boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> Eval LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [a] = throwError $ TypeMismatch "pair" a
car a = throwError $ NumArgs 1 a

cdr :: [LispVal] -> Eval LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList _ x] = return x
cdr [a] = throwError $ TypeMismatch "pair" a
cdr a = throwError $ NumArgs 1 a

cons :: [LispVal] -> Eval LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs x'] = return $ DottedList (x:xs) x'
cons [x, x'] = return $ DottedList [x] x'
cons a = throwError $ NumArgs 2 a

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Eval LispVal
numericBinop op args = liftM (Number . foldl1 op) (mapM unpackNum args)

boolBinop
  :: (LispVal -> Eval a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> Eval LispVal
boolBinop f op [a,b] = liftM Bool $ liftM2 op (f a) (f b)
boolBinop _ _ args = throwError $ NumArgs 2 args

unpackNum :: LispVal -> Eval Integer
unpackNum (Number n) = return n
unpackNum a = throwError $ TypeMismatch "integer" a

unpackStr :: LispVal -> Eval String
unpackStr (String s) = return s
unpackStr a = throwError $ TypeMismatch "string" a

unpackBool :: LispVal -> Eval Bool
unpackBool (Bool b) = return b
unpackBool a = throwError $ TypeMismatch "boolean" a

makePort :: IOMode -> [LispVal] -> Eval LispVal
makePort mode [String s] = liftM Port $ liftIO $ openFile s mode
makePort _ [a]  = throwError $ TypeMismatch "string" a
makePort _ a    = throwError $ NumArgs 1 a

closePort :: [LispVal] -> Eval LispVal
closePort [Port h] = liftIO $ hClose h >> return (Bool True)
closePort [a] = throwError $ TypeMismatch "port" a
closePort a   = throwError $ NumArgs 1 a

readProc :: [LispVal] -> Eval LispVal
readProc [] = readProc [Port stdin]
readProc [Port h] = liftIO (hGetLine h) >>= readExpr
readProc [a] = throwError $ TypeMismatch "port" a
readProc a   = throwError $ NumArgs 1 a

writeProc :: [LispVal] -> Eval LispVal
writeProc [v] = writeProc [v, Port stdout]
writeProc [v, Port h] = liftIO (hPrint h v) >> return (Bool True)
writeProc [_, a]  = throwError $ TypeMismatch "port" a
writeProc a       = throwError $ NumArgs 2 a

readContents :: [LispVal] -> Eval LispVal
readContents [String s] = liftM String $ liftIO $ readFile s
readContents [a]  = throwError $ TypeMismatch "string" a
readContents a    = throwError $ NumArgs 1 a

readAll :: [LispVal] -> Eval LispVal
readAll [String s] = liftM List $ load s
readAll [a] = throwError $ TypeMismatch "string" a
readAll a   = throwError $ NumArgs 1 a
