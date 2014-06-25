{-# LANGUAGE TupleSections #-}

module Stack
  (
    LispStack(..)
  , peel
  , getVar
  , setVar
  , defVar
  , initial
  ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M

import Types


peel :: LispStack -> LispStack
peel (Frame _ env)  = env
peel env            = env

getVar :: String -> Eval LispVal
getVar k = get >>= getVar' k

getVar' :: String -> LispStack -> Eval LispVal
getVar' k (Initial m) = maybe
  (throwError $ UnboundVar "Getting an unbound variable" k)
  return
  (M.lookup k m)
getVar' k (Frame m stack) = maybe
  (getVar' k stack)
  return
  (M.lookup k m)

setVar :: String -> LispVal -> Eval LispVal
setVar k v = get >>= setVar' k v >>= put >> return v

setVar' :: String -> LispVal -> LispStack -> Eval LispStack
setVar' k v (Initial m) = if M.member k m
  then return $ Initial (M.insert k v m)
  else throwError $ UnboundVar "Setting an unbound variable" k
setVar' k v (Frame m o) = if M.member k m
  then return $ Frame (M.insert k v m) o
  else Frame m <$> setVar' k v o

defVar :: String -> LispVal -> Eval LispVal
defVar k v = get >>= go >>= put >> return v where
  go (Initial m) = return $ Initial (M.insert k v m)
  go (Frame m o) = return $ Frame (M.insert k v m) o

initial :: LispStack
initial = Initial M.empty
