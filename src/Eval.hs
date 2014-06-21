{-# LANGUAGE TupleSections #-}

module Eval
  (
    eval
  , readExpr
  ) where

import Control.Monad.Error
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

import Data
import Error
import Lib
import Parser
import Stack

readExpr :: String -> ThrowsError LispVal
readExpr s = case parse parseExpr "lisp" s of
  Left err -> throwError $ Parser err
  Right v -> return v

eval :: LispStack -> LispVal -> ThrowsError (LispVal, LispStack)
eval env v@(String _) = return (v, env)
eval env v@(Number _) = return (v, env)
eval env v@(Bool _)   = return (v, env)
eval env (List [Atom "quote", val]) = return (val, env)
eval env (List [Atom "if", p, t, f]) = do
  (result, env') <- eval env p
  case result of
    Bool True -> eval env' t
    Bool _ -> eval env' f
    a -> throwError $ TypeMismatch "boolean" a
eval env (Atom k) = get k env
eval env (List [Atom "set!", Atom k, expr]) =
  eval env expr >>= uncurry (set k)
eval env (List [Atom "define", Atom k, expr]) =
  eval env expr >>= uncurry (def k)
eval env (List (Atom k : args)) = mapStack env args >>= \(a,e) -> apply e k a
eval _ a = throwError $ BadSpecialForm "Unrecognised special form" a

mapStack :: LispStack -> [LispVal] -> ThrowsError ([LispVal], LispStack)
mapStack env [] = return ([], env)
mapStack env (v:vs) = do
  (v', env') <- eval env v
  (vs', env'') <- mapStack env' vs
  return (v':vs', env'')

apply :: LispStack -> String -> [LispVal] -> ThrowsError (LispVal, LispStack)
apply env k args = maybe
  (throwError $ NotFunction "Unrecognised primitive function" k)
  (liftM (,env) . ($ args))
  (M.lookup k lib)
