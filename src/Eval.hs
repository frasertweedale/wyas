{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Eval
  (
    eval
  , readExpr
  ) where

import Control.Monad.Error
import qualified Data.Map as M

import Text.ParserCombinators.Parsec

import Types
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
eval env (List (Atom "define" : List (Atom k : params) : body)) =
  let  -- ensure function environment has itself, i.e. to allow recursion
    f = fun env' params body
    env' = case env of
      Initial m -> Initial (M.insert k f m)
      Frame m stack -> Frame (M.insert k f m) stack
  in def k f env'
-- TODO "define" DottedList
eval env (List (Atom "lambda" : List params : body)) =
  return (fun env params body, env)
-- TODO "lambda" DottedList
-- TODO "lambda" atom varargs
eval env (List (f : args)) = do
  (f', env') <- eval env f
  (args', env'') <- mapStack env' args
  liftM (,env'') (apply f' args')
eval _ a = throwError $ BadSpecialForm "Unrecognised special form" a

mapStack :: LispStack -> [LispVal] -> ThrowsError ([LispVal], LispStack)
mapStack env [] = return ([], env)
mapStack env (v:vs) = do
  (v', env') <- eval env v
  (vs', env'') <- mapStack env' vs
  return (v':vs', env'')

apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (PrimFun f) args = f args
apply (Fun {..}) args =
  if length args /= length params
    then throwError $ NumArgs (length params) args
    else liftM (last . fst)
      $ mapStack (Frame (M.fromList (zip params args)) closure) body
apply v args = throwError $ NotFunction (show v) (show args)

-- | Construct a function
--
-- TODO: ensure params are atoms only, else throw
--
fun :: LispStack -> [LispVal] -> [LispVal] -> LispVal
fun env params body = Fun (map show params) body env
