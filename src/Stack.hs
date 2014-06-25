{-# LANGUAGE TupleSections #-}

module Stack
  (
    LispStack(..)
  , peel
  , get
  , set
  , def
  , initial
  ) where

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M

import Types


peel :: LispStack -> LispStack
peel (Frame _ env)  = env
peel env            = env

get :: String -> LispStack -> ThrowsError (LispVal, LispStack)
get k env@(Initial m) = maybe
  (throwError $ UnboundVar "Getting an unbound variable" k)
  (return . (,env))
  (M.lookup k m)
get k env@(Frame m stack) = maybe
  (second (const env) <$> get k stack)
  (return . (,env))
  (M.lookup k m)

set :: String -> LispVal -> LispStack -> ThrowsError (LispVal, LispStack)
set k v (Initial m) = if M.member k m
  then return (v, Initial (M.insert k v m))
  else throwError $ UnboundVar "Setting an unbound variable" k
set k v (Frame m o) = if M.member k m
  then return (v, Frame (M.insert k v m) o)
  else second (Frame m) <$> set k v o

def :: String -> LispVal -> LispStack -> ThrowsError (LispVal, LispStack)
def k v stack = case stack of
  Initial m -> return (v, Initial (M.insert k v m))
  Frame m o -> return (v, Frame (M.insert k v m) o)

initial :: LispStack
initial = Initial M.empty
