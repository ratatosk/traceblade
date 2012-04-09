{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

module Match ( Bindings
             , Env(..)
             , eTid
             , eSyscall
             , eRetcode
             , eArgs
             , Value(..)
             , Matcher
             , runMatcher
             , unA
             , unB
             , unS
             , unN 
             , Pack
             , pack
             , unpack
             , parseMatch
             ) where

import Control.Applicative hiding ((<|>))

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.Accessor
import Data.Accessor.Template

import qualified Data.Map as M

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import ParserUtils

import Syscall

data Value = B Bool | A String | N Int | S String | L [Value] 
           deriving (Eq, Show)

type Bindings = M.Map String Value

data Env = Env { eTid_ :: Value
               , eSyscall_ :: Value
               , eRetcode_ :: Value
               , eArgs_ :: Value
               }
           
$(deriveAccessors ''Env)

newtype Matcher a = Matcher { unMatcher :: ErrorT String (StateT Bindings (Reader Env)) a }
                  deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadReader Env
                           , MonadError String
                           , MonadState Bindings)

runMatcher :: Matcher a -> Int -> Syscall -> Bindings -> (Either String a, Bindings)
runMatcher m t s b = runReader (runStateT (runErrorT (unMatcher m)) b) (mkEnv t s)

unB :: Value -> Matcher Bool
unB (B x) = return x
unB x = throwError $ "Expected boolean, got " ++ show x

unN :: Value -> Matcher Int
unN (N x) = return x
unN x = throwError $ "Expected integer, got " ++ show x

unS :: Value -> Matcher String
unS (S x) = return x
unS x = throwError $ "Expected string, got " ++ show x

unA :: Value -> Matcher String
unA (A x) = return x
unA x = throwError $ "Expected atom, got " ++ show x

unL :: Value -> Matcher [Value]
unL (L x) = return x
unL x = throwError $ "Expected list, got " ++ show x

class Pack a where
  pack :: a -> Value
  unpack :: Value -> Matcher a
  
instance Pack Int where
  pack = N
  unpack = unN
  
instance Pack [Char] where
  pack = S
  unpack = unS
  
instance Pack Bool where
  pack = B
  unpack = unB
  
typeN :: Value
typeN =  A "number"

typeS :: Value
typeS =  A "string"

typeM :: Value
typeM =  A "mask"

typeL :: Value
typeL =  A "labelled"

typeO :: Value
typeO = A "object"

encode :: Argument -> Value
encode (NumLiteral n) = L [typeN, N n]
encode (StrLiteral s) = L [typeS, S s]
encode (Mask strs) = L (typeM : map A strs)
encode (Labelled l a) = L [typeL, A l, encode a] 
encode (Object a) = L (typeO : map encode a)

mkEnv :: Int -> Syscall -> Env
mkEnv t s = Env { eTid_ = N t
                , eSyscall_ = S (getVal scName s)
                , eRetcode_ = N (getVal scRet s)
                , eArgs_ = L (map encode $ getVal scArgs s)
                }

boolLit :: Parser Bool
boolLit = char '#' >> ((char 'f' >> return False) <|> (char 't' >> return True))

expr :: Parser Value
expr = (B <$> boolLit)    <|>
       (A <$> identifier) <|> 
       (N <$> number)     <|>
       (S <$> quoted)     <|>
       (L <$> list)
       
list :: Parser [Value]
list = (char '(' >> spaces) *> manyTill (expr <* spaces) (char ')')
       
parseMatch :: String -> Either String Value
parseMatch s = case parse expr "" s of
  Left e -> Left $ show e
  Right s -> Right s 