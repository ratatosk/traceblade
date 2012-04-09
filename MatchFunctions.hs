module MatchFunctions (function) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Accessor

import qualified Data.Map as M

import Util
import Match
import MatchUtil
import Syscall

type Function = [Matcher Value] -> Matcher Value
type Constant = Matcher Value

-- functions that get their args as monadic values and can discard 
-- monadic effects by ignoring the argument
lazyFuncs :: [(String, Function)]
lazyFuncs = [ ("if", lif)
            , ("and", land)
            , ("or", lor)
            , ("cond", cond)
            ]
            
-- functions that get their args as pure values with all monadic effects already applied
strictFuncs :: [(String, Function)]
strictFuncs = map (mapSnd strict)
              [ ("eq", eq)
              , ("progn", progn)
              , ("stringp", args1 isString)
              , ("boolp", args1 isBool)
              , ("nump", args1 isNum)
              , ("tostring", args1 toString)
              , ("tobool", args1 toBool)
              , ("error", args1 err) 
              , ("suffix", args2 . wrap2 . pure2 $ strSuffix)
              , ("infix", args2 . wrap2 . pure2 $ strInfix)
              , ("prefix", args2 . wrap2 . pure2 $ strPrefix)
              , ("get", args1 bGet)
              , ("set", args2 bSet)
              , ("unset", args1 bUnset)
              , ("isset", args1 bIsSet)
              , ("add", wrapl add)
              , ("sub", wrapl sub)
              ]

constants :: [(String, Function)]
constants = map (mapSnd constant)
            [ ("tid", tid)
            , ("syscall", syscall)
            , ("retcode", retcode)
            , ("args", args)
            ]

functions :: M.Map String Function
functions = M.fromList $ lazyFuncs ++ strictFuncs ++ constants
            
function :: String -> Matcher Function
function fn = maybe (throwError $ "Unbound function: " ++ fn) return $ M.lookup fn functions

constant :: Matcher Value -> Function
constant m [] = m
constant _ _ = throwError "Cannot apply constant to arguments"

--------------------------------------------------------------------------------
-- Type predicates
--------------------------------------------------------------------------------

isString :: Value -> Matcher Value
isString (S _) = return $ B True
isString _ = return $ B False

isBool :: Value -> Matcher Value
isBool (B _) = return $ B True
isBool _ = return $ B False

isNum :: Value -> Matcher Value
isNum (N _) = return $ B True
isNum _ = return $ B False

--------------------------------------------------------------------------------
-- String functions
--------------------------------------------------------------------------------

strInfix :: String -> String -> Bool
strInfix = isInfixOf

strPrefix :: String -> String -> Bool
strPrefix = isPrefixOf

strSuffix :: String -> String -> Bool
strSuffix = isSuffixOf


toString :: Value -> Matcher Value
toString (S s) = return $ S s
toString (B b) = return $ S (show b)
toString (N n) = return $ S (show n)
toString (A i) = return $ S (show i)
toString (L l) = return $ S (show l)

--------------------------------------------------------------------------------
-- Control flow
--------------------------------------------------------------------------------

err :: Value -> Matcher Value
err msg = unS msg >>= throwError
  
lif :: [Matcher Value] -> Matcher Value
lif [c, t, f] = do
  v <- c >>= unpack
  if v then t else f
lif _ = throwError "'if' requires three arguments"

cond :: [Matcher Value] -> Matcher Value
cond [] = throwError "'cond' was non-exhaustive"
cond [x] = throwError "'cond' need even number of arguments"
cond (c:d:cs) = do
  v <- c >>= unpack
  if v then d else cond cs
  
progn :: [Value] -> Matcher Value
progn [] = throwError "'progn' with empty body"
progn x = return $ last x

--------------------------------------------------------------------------------
-- Logic functions
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

eq :: [Value] -> Matcher Value
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

--------------------------------------------------------------------------------
-- Binding functions
--------------------------------------------------------------------------------

bGet :: Value -> Matcher Value
bGet x = do
  x' <- unA x
  v <- M.lookup x' <$> get
  case v of
    Just v -> return v
    Nothing -> throwError $ "Variable '" ++ x' ++ "' is unbound"

bSet :: Value -> Value -> Matcher Value
bSet k v = do
  k' <- unA k
  m <- get
  let 
    comb _ n _ = n
    (r, m') = mapFst (B . isJust) $ M.insertLookupWithKey comb k' v m
  put m'
  return r

bUnset :: Value -> Matcher Value
bUnset k = do
  k' <- unA k
  m <- get
  let 
    upd _ _ = Nothing
    (r, m') = mapFst (B . isJust) $ M.updateLookupWithKey upd k' m
  put m'
  return r

bIsSet :: Value -> Matcher Value
bIsSet k = do
  k' <- unA k
  B . isJust . M.lookup k' <$> get
    
--------------------------------------------------------------------------------
-- Syscall access functions
--------------------------------------------------------------------------------
  
retcode :: Constant
retcode = asks $ getVal eRetcode

syscall :: Constant
syscall = asks $ getVal eSyscall

args :: Constant
args = asks $ getVal eArgs

tid :: Constant
tid = asks $ getVal eTid

--------------------------------------------------------------------------------
-- Arithmitic functions
--------------------------------------------------------------------------------

add :: [Int] -> Matcher Int
add xs = return $ sum xs

sub :: [Int] -> Matcher Int
sub [] = throwError "'sub' requires at least one argument"
sub (x:xs) = return $ x - sum xs

