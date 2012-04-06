{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MatchEval (match) where

import Data.Accessor

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

eval :: Value -> Matcher Value
eval (L (x:xs)) = apply x xs
eval (A n) = join $ constant n
eval x = return x

apply :: Value -> [Value] -> Matcher Value
apply (A fn) args = function fn >>= (=<< mapM eval args)
apply a _ = throwError $ "Cannot apply non-function: " ++ show a

match :: Int -> Syscall -> Value -> Either String Bool
match t s e = runMatcher (eval e >>= conclude) t s