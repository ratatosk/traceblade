module MatchFunctions (function, constant) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader

import Data.List
import Data.Accessor

import qualified Data.Map as M

import Match
import MatchUtil
import Syscall

type Function = [Matcher Value] -> Matcher Value
type Constant = Matcher Value

lazyFuncs :: [(String, Function)]
lazyFuncs = [ ("if", iff)
            , ("and", bop and)
            , ("or", bop or)
            ]

functions :: M.Map String Function
functions = M.fromList $ mapSnd strict 
            [ ("eq", eq)
            , ("stringp", args1 isString)
            , ("boolp", args1 isBool)
            , ("nump", args1 isNum)
            , ("tostring", args1 toString)
            , ("tobool", args1 toBool)
            , ("error", args1 err) 
            , ("suffix", wrap2 . pure2 $ strSuffix)
            , ("infix", wrap2 . pure2 $ strInfix)
            , ("prefix", wrap2 . pure2 $ strPrefix)
            ]

           
constants :: M.Map String Constant
constants = M.fromList [ ("retcode", retcode)
                       , ("syscall", syscall)
                       , ("tid", tid)
                       ]
            
mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f ((a, b):xs) = (a, f b) : mapSnd f xs
mapSnd _ [] = []
             
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

strInfix :: String -> String -> Bool
strInfix = isInfixOf

strPrefix :: String -> String -> Bool
strPrefix = isPrefixOf

strSuffix :: String -> String -> Bool
strSuffix = isSuffixOf

err :: Value -> Matcher Value
err msg = unS msg >>= throwError

bop :: ([Bool] -> Bool) -> [Value] -> Matcher Value
bop f args = do
  bools <- mapM unpack args
  return $ B $ f bools

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

{-
args :: Constant
args = packList . getVal scArgs <$> asks meSyscall
  where
    packList = L $ map pack
-}

tid :: Constant
tid = N <$> asks meTid
