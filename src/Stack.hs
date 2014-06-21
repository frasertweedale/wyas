{-# LANGUAGE TupleSections #-}

module Stack
  (
    LispStack(..)
  , get
  , set
  , def
  , initial
  ) where

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M

import Types


isBound :: String -> LispStack -> Bool
isBound k (Initial m) = M.member k m
isBound k (Frame m _) = M.member k m  -- don't recurse

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
set k v stack
  | isBound k stack = case stack of
    Initial m -> return (v, Initial (M.insert k v m))
    Frame m o -> return (v, Frame (M.insert k v m) o)
  | otherwise = throwError $ UnboundVar "Setting an unbound variable" k

def :: String -> LispVal -> LispStack -> ThrowsError (LispVal, LispStack)
def k v stack = case stack of
  Initial m -> return (v, Initial (M.insert k v m))
  Frame m o -> return (v, Frame (M.insert k v m) o)

initial :: LispStack
initial = Initial M.empty
