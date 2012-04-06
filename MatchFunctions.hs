module MatchFunctions (function, constant) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

import Data.List
import Data.Accessor

import qualified Data.Map as M

import Match
import Syscall

type Function = [Value] -> Matcher Value
type Constant = Matcher Value

functions :: M.Map String Function
functions = M.fromList [ ("eq", eq)
                       , ("and", bop and)
                       , ("or", bop or)
                       , ("stringp", isString)
                       , ("boolp", isBool)
                       , ("nump", isNum)
                       , ("tostring", args1 toString)
                       , ("tobool", args1 toBool)
                       , ("error", args1 err) 
                       , ("suffix", wrap2p strSuffix)
                       , ("infix", wrap2p strInfix)
                       , ("prefix", wrap2p strPrefix)
                       ]
           
constants :: M.Map String Constant
constants = M.fromList [ ("retcode", retcode)
                       , ("syscall", syscall)
                       , ("tid", tid)
                       ]
            
isString :: Value -> Matcher Value
isString (S _) = return $ B True
isString _ = return $ B False

isBool :: Value -> Matcher Value
isBool (B _) = return $ B True
isBool _ = return $ B False

isNum :: Value -> Matcher Value
isNum (N _) = return $ B True
isNum _ = return $ B False

function :: String -> Matcher Function
function fn = maybe (throwError $ "Unbound function: " ++ fn) return $ M.lookup fn functions

constant :: String -> Matcher Constant
constant cn = maybe (throwError $ "Unbound constant: " ++ cn) return $ M.lookup cn constants

args1 :: (Value -> Matcher Value) -> Function
args1 f [a] = f a 
args1 f _   = throwError "1 argument required"
  
args2 :: (Value -> Value -> Matcher Value) -> Function
args2 f [a, b] = f a b
args2 f _      = throwError "2 arguments required"

pure1 :: (a -> b) -> a -> Matcher b
pure1 f = return . f

pure2 :: (a -> b -> c) -> a -> b -> Matcher c
pure2 f x y = return $ f x y

wrap1 :: (Pack a, Pack b) => (a -> Matcher b) -> Function
wrap1 = args1 . wrap1' 
  where 
    wrap1' f x = pack <$> (unpack x >>= f)
    
wrap1p :: (Pack a, Pack b) => (a -> b) -> Function
wrap1p = wrap1 . pure1

wrap2 :: (Pack a, Pack b, Pack c) => (a -> b -> Matcher c) -> Function
wrap2 = args2 . wrap2'
  where
    wrap2' f x y = do 
      x' <- unpack x
      y' <- unpack y
      pack <$> f x' y'

wrap2p :: (Pack a, Pack b, Pack c) => (a -> b -> c) -> Function
wrap2p = wrap2 . pure2

strInfix :: String -> String -> Bool
strInfix = isInfixOf

strPrefix :: String -> String -> Bool
strPrefix = isPrefixOf

strSuffix :: String -> String -> Bool
strSuffix = isSuffixOf

err :: Value -> Matcher Value
err msg = unS msg >>= throwError

bop :: ([Bool] -> Bool) -> Function
bop f args = do
  bools <- mapM unpack args
  return $ B $ f bools

eq :: Function
eq x | length x < 2 = throwError "Builtin function 'eq' requires at least 2 arguments"
     | otherwise = if and (zipWith (==) x (tail x))
       then return $ B True
       else return $ B False
     
toBool :: Value -> Matcher Value
toBool x = case x of
  (B False) -> f
  (S "")    -> f
  (N 0)     -> f
  (L [])    -> f
  _         -> t
  where 
    f = return $ B False
    t = return $ B True

toString :: Value -> Matcher Value
toString (S s) = return $ S s
toString (B b) = return $ S (show b)
toString (N n) = return $ S (show n)
toString (A i) = return $ S (show i)
toString (L l) = return $ S (show l)

retcode :: Constant
retcode = N . getVal scRet <$> asks meSyscall

syscall :: Constant
syscall = S . getVal scName <$> asks meSyscall

packList :: Arguments

args :: Constant
args = packList . getVal scArgs <$> asks meSyscall
  where
    packList = L $ map pack

tid :: Constant
tid = N <$> asks meTid
