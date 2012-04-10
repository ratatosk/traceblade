{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module MatchEval (match) where

import Data.Accessor
import Data.Accessor.Basic

import qualified Data.Map as M

import Control.Applicative

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

import Syscall
import Match
import MatchFunctions

conclude :: Value -> Matcher Bool
conclude (B True) = return True
conclude (B False) = return False
conclude x = throwError $ "Type mismatch, BoolAtom was expected instead of: " ++ show x

--------------------------------------------------------------------------------
-- Special functions that evaluate their arguments in non-uniform way:
--------------------------------------------------------------------------------

land :: [Matcher Value] -> Matcher Value
land [] = return $ B True
land (m:ms) = do
  v <- m >>= unB
  if v then land ms else return (B False)
  
lor :: [Matcher Value] -> Matcher Value
lor [] = return $ B False
lor (m:ms) = do
  v <- m >>= unB
  if v then return (B True) else lor ms

lif :: [Matcher Value] -> Matcher Value
lif [c, t, f] = do
  v <- c >>= unpack
  if v then t else f
lif _ = throwError "'if' requires three arguments"

cond :: [Value] -> Matcher Value
cond (L [c, s] : cs) = do
  c' <- eval c >>= unB
  if c' then eval s else cond cs
cond [] = throwError "'cond' non exhaustive"
cond _ = throwError "'cond' requires list of pairs (condition statement)"

quote :: [Value] -> Matcher Value
quote [x] = return x
quote _  = throwError "'quote' needs exactly one argument"

bindMap :: [Value] -> Matcher Bindings
bindMap bs = M.fromList <$> mapM getBind bs
  where
    getBind (L [A k, v]) = (k,) <$> eval v
    getBind _ = throwError "'let' binding pair have atom as its first part" 

blet :: [Value] -> Matcher Value
blet [L bs, b] = do
  bm <- bindMap bs
  local (modify eLocals $ \m -> M.union bm m) $ eval b
blet _ = throwError "'let' requires list of binding pairs and body expression"
  
var :: String -> Matcher Value
var n = do
  v <- M.lookup n <$> asks (getVal eLocals)
  maybe (throwError $ "Unbound variable: " ++ n) return v

eval :: Value -> Matcher Value
eval (L (x:xs)) = apply x xs
eval (A n) = var n
eval x = return x

apply :: Value -> [Value] -> Matcher Value
apply (A "and") args = land $ map eval args
apply (A "or") args = lor $ map eval args
apply (A "if") args = lif $ map eval args
apply (A "cond") args = cond args
apply (A "quote") args = quote args
apply (A "let") args = blet args
apply (A fn) args = function fn >>= (=<< mapM eval args)
apply a _ = throwError $ "Cannot apply non-function: " ++ show a

match :: Int -> Syscall -> Value -> Bindings -> (Either String Bool, Bindings)
match t s e b = runMatcher (eval e >>= conclude) t s b
