{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Eval
  (
    eval
  , readExpr
  ) where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M

import qualified Text.ParserCombinators.Parsec as Parsec

import Types
import Parser
import Stack

readExpr :: String -> Eval LispVal
readExpr s = case Parsec.parse parseExpr "lisp" s of
  Left err -> throwError $ Parser err
  Right v -> return v

eval :: LispVal -> Eval LispVal
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Bool _)   = return v
eval (List [Atom "quote", v]) = return v
eval (List [Atom "if", p, t, f]) = do
  v <- eval p
  case v of
    Bool True -> eval t
    Bool _    -> eval f
    a         -> throwError $ TypeMismatch "boolean" a
eval (Atom k) = getVar k
eval (List [Atom "set!", Atom k, expr]) = eval expr >>= setVar k
eval (List [Atom "define", Atom k, expr]) = eval expr >>= defVar k
eval (List (Atom "define" : List (Atom k : params) : body)) = do
  env <- get
  let  -- ensure function environment has itself, i.e. to allow recursion
    f = Fun (map show params) body env'
    env' = case env of
      Initial m -> Initial (M.insert k f m)
      Frame m stack -> Frame (M.insert k f m) stack
  put env' >> defVar k f
-- TODO "define" DottedList
eval (List (Atom "lambda" : List params : body)) = do
  env <- get
  return (Fun (map show params) body env)
-- TODO "lambda" DottedList
-- TODO "lambda" atom varargs
eval (List (f@(Atom k) : args)) = do
  f' <- eval f
  args' <- mapStack args
  (f'', v) <- apply f' args'
  setVar k f'' >> return v
eval a = throwError $ BadSpecialForm "Unrecognised special form" a

mapStack :: [LispVal] -> Eval [LispVal]
mapStack = foldr (liftM2 (:) . eval) (return [])

apply :: LispVal -> [LispVal] -> Eval (LispVal, LispVal)
apply v@(PrimFun f) args = liftM (v,) (f args)
apply (Fun {..}) args =
  if length args /= length params
    then throwError $ NumArgs (length params) args
    else do
      -- TODO inner runEval probably nicer than this state munging
      callerEnv <- get
      _ <- put $ Frame (M.fromList (zip params args)) closure
      vals <- mapStack body
      env' <- get
      put callerEnv
      return (Fun params body (peel env'), last vals)  -- FIXME non-total
apply v args = throwError $ NotFunction (show v) (show args)
