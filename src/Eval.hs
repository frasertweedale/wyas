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
    f = Fun (map show params) body env'
    env' = case env of
      Initial m -> Initial (M.insert k f m)
      Frame m stack -> Frame (M.insert k f m) stack
  in def k f env'
-- TODO "define" DottedList
eval env (List (Atom "lambda" : List params : body)) =
  return (Fun (map show params) body env, env)
-- TODO "lambda" DottedList
-- TODO "lambda" atom varargs
eval env (List (f@(Atom k) : args)) = do
  (f', env') <- eval env f
  (args', env'') <- mapStack env' args
  (f'', v) <- apply f' args'
  liftM ((v,) . snd) (set k f'' env'')
eval _ a = throwError $ BadSpecialForm "Unrecognised special form" a

mapStack :: LispStack -> [LispVal] -> ThrowsError ([LispVal], LispStack)
mapStack env [] = return ([], env)
mapStack env (v:vs) = do
  (v', env') <- eval env v
  (vs', env'') <- mapStack env' vs
  return (v':vs', env'')

apply :: LispVal -> [LispVal] -> ThrowsError (LispVal, LispVal)
apply v@(PrimFun f) args = liftM (v,) (f args)
apply (Fun {..}) args =
  if length args /= length params
    then throwError $ NumArgs (length params) args
    else do
      let env = Frame (M.fromList (zip params args)) closure
      (vals, env') <- mapStack env body
      return (Fun params body (peel env'), last vals)  -- FIXME non-total
apply v args = throwError $ NotFunction (show v) (show args)
