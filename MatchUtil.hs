module MatchUtil where

import Control.Applicative

import Control.Monad
import Control.Monad.Error

import Match

strict :: ([a] -> Matcher b) -> [Matcher a] -> Matcher b
strict f a = sequence a >>= f

args1 :: (Value -> Matcher Value) -> [Value] -> Matcher Value
args1 f [a] = f a 
args1 f _   = throwError "1 argument required"
  
args2 :: (Value -> Value -> Matcher Value) -> [Value] -> Matcher Value
args2 f [a, b] = f a b
args2 f _      = throwError "2 arguments required"

pure1 :: (a -> b) -> a -> Matcher b
pure1 f = return . f

pure2 :: (a -> b -> c) -> a -> b -> Matcher c
pure2 f x y = return $ f x y

wrap1 :: (Pack a, Pack b) => (a -> Matcher b) -> Value -> Matcher Value
wrap1 f x = pack <$> (unpack x >>= f)
    
wrap2 :: (Pack a, Pack b, Pack c) => (a -> b -> Matcher c) -> [Value] -> Matcher Value
wrap2 f x y = do 
  x' <- unpack x
  y' <- unpack y
  pack <$> f x' y'

