{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Logic (match) where

import qualified Data.Map as M

import Data.Accessor

import Control.Applicative

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

import Syscall
import Rules

data MatcherEnv = MatcherEnv { meTid :: Int
                             , meSyscall :: Syscall
                             }

-- May be extended to ErrorT RWS if logging and environment (for lambda abstraction) will be added
newtype Matcher a = Matcher { unMatcher :: ErrorT String (Reader MatcherEnv) a }
                  deriving (Functor, Monad, MonadReader MatcherEnv, MonadError String)

runMatcher :: Matcher a -> Int -> Syscall -> Either String a
runMatcher m t s = runReader (runErrorT (unMatcher m)) $ MatcherEnv t s

type Builtin = [Expr] -> Matcher Expr
type Unary = Expr -> Matcher Expr
type Nullary = Matcher Expr

builtins :: M.Map String Builtin
builtins = M.fromList [ ("eq", eq)
                      , unary "tostring" toString
                      , unary "tobool" toBool
                      , nullary "retcode" retcode
                      , nullary "syscall" syscall
                      , nullary "tid" tid
                      ]

eq :: Builtin
eq x | length x >= 2 = case and (zipWith (==) x (tail x)) of
  True -> return $ BoolAtom True
  False -> return $ BoolAtom False
     | otherwise = fail "Builtin function 'eq' requires at least 2 arguments"

unary :: String -> (Expr -> Matcher Expr) -> (String, Builtin)
unary n x = (n, op)
  where
    op = \args -> do
      when (length args /= 1) $ error $ "Builtin function " ++ n ++ " requires 1 argument"
      x $ head args
      
nullary :: String -> Matcher Expr -> (String, Builtin)
nullary n x = (n, op)
  where
    op = \args -> do
      when (not $ null args) $ error $ "Builtin function " ++ n ++ " doesn't require arguments"
      x
     
toBool :: Unary
toBool x = case x of
  (BoolAtom False) -> f
  (StrAtom "")     -> f
  (NumAtom 0)      -> f
  (List [])        -> f
  _                -> t
  where 
    f = return $ BoolAtom False
    t = return $ BoolAtom True

toString :: Unary
toString (StrAtom s) = return $ StrAtom s
toString (BoolAtom b) = return $ StrAtom (show b)
toString (NumAtom n) = return $ StrAtom (show n)
toString (IdAtom i) = return $ StrAtom (show i)
toString (List l) = return $ StrAtom (show l)

retcode :: Nullary
retcode = NumAtom . getVal scRet <$> asks meSyscall

syscall :: Nullary
syscall = StrAtom . getVal scName <$> asks meSyscall

tid :: Nullary
tid = NumAtom <$> asks meTid

conclude :: Expr -> Matcher Bool
conclude (BoolAtom True) = return True
conclude (BoolAtom False) = return False
conclude x = fail $ "Type mismatch, BoolAtom was expected instead of: " ++ show x

eval :: Expr -> Matcher Expr
eval (List (x:xs)) = apply x xs
eval x = return x

apply :: Expr -> [Expr] -> Matcher Expr
apply (IdAtom func) args = case M.lookup func builtins of
  Nothing -> fail $ "Function not found: " ++ func
  Just func -> func args
apply a _ = fail $ "Cannot apply non-function: " ++ show a

match :: Int -> Syscall -> Expr -> Either String Bool
match t s e = runMatcher (eval e >>= conclude) t s
