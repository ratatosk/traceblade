{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Match ( Env(..)
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
import Control.Monad.Error

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import ParserUtils

import Syscall

data Env = Env { meTid :: Int
               , meSyscall :: Syscall
               }

data Value = B Bool | A String | N Int | S String | L [Value] 
           deriving (Eq, Show)

newtype Matcher a = Matcher { unMatcher :: ErrorT String (Reader Env) a }
                  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

runMatcher :: Matcher a -> Int -> Syscall -> Either String a
runMatcher m t s = runReader (runErrorT (unMatcher m)) $ Env t s

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

expr :: Parser Value
expr = (A <$> identifier) <|> 
       (N <$> number)     <|>
       (S <$> quoted)     <|>
       (L <$> list)
       
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
  
list :: Parser [Value]
list = (char '(' >> spaces) *> manyTill (expr <* spaces) (char ')')

parseMatch :: String -> Either String Value
parseMatch s = case parse expr "" s of
  Left e -> Left $ show e
  Right s -> Right s