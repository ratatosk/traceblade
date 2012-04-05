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

data Env = Env { meTid :: Int
               , meSyscall :: Syscall
               }

-- May be extended to ErrorT RWS if logging and environment (for lambda abstraction) will be added
newtype Matcher a = Matcher { unMatcher :: ErrorT String (Reader Env) a }
                  deriving (Functor, Monad, MonadReader Env, MonadError String)

runMatcher :: Matcher a -> Int -> Syscall -> Either String a
runMatcher m t s = runReader (runErrorT (unMatcher m)) $ Env t s

type Function = [Value] -> Matcher Value
type Constant = Matcher Value

type Unary = Value -> Matcher Value

functions :: M.Map String Function
functions = M.fromList [ ("eq", eq)
                       , unary "tostring" toString
                       , unary "tobool" toBool
                       ]
           
constants :: M.Map String Constant
constants = M.fromList [ ("retcode", retcode)
                       , ("syscall", syscall)
                       , ("tid", tid)
                       ]

eq :: Function
eq x | length x < 2 = throwError "Builtin function 'eq' requires at least 2 arguments"
eq x | otherwise = case and (zipWith (==) x (tail x)) of
  True -> return $ B True
  False -> return $ B False

unary :: String -> (Value -> Matcher Value) -> (String, Function)
unary n x = (n, op)
  where
    op = \args -> do
      when (length args /= 1) $ error $ "Builtin function " ++ n ++ " requires 1 argument"
      x $ head args
     
toBool :: Unary
toBool x = case x of
  (B False) -> f
  (S "")    -> f
  (N 0)     -> f
  (L [])    -> f
  _         -> t
  where 
    f = return $ B False
    t = return $ B True

toString :: Unary
toString (S s) = return $ S s
toString (B b) = return $ S (show b)
toString (N n) = return $ S (show n)
toString (A i) = return $ S (show i)
toString (L l) = return $ S (show l)

retcode :: Constant
retcode = N . getVal scRet <$> asks meSyscall

syscall :: Constant
syscall = S . getVal scName <$> asks meSyscall

tid :: Constant
tid = N <$> asks meTid

conclude :: Value -> Matcher Bool
conclude (B True) = return True
conclude (B False) = return False
conclude x = fail $ "Type mismatch, BoolAtom was expected instead of: " ++ show x

eval :: Value -> Matcher Value
eval (L (x:xs)) = apply x xs
eval (A n) = maybe (throwError $ "Unknown constant: " ++ n) id $ M.lookup n constants
eval x = return x

apply :: Value -> [Value] -> Matcher Value
apply (A func) args = case M.lookup func functions of
  Nothing -> fail $ "Function not found: " ++ func
  Just f -> f =<< mapM eval args
apply a _ = fail $ "Cannot apply non-function: " ++ show a

match :: Int -> Syscall -> Value -> Either String Bool
match t s e = runMatcher (eval e >>= conclude) t s
