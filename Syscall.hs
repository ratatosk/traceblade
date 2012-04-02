{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Syscall ( Syscall(..)
               , scName
               , scArgs
               , scRet
               , Argument(..)
               , parseSyscall
               ) where

import Control.Monad
import Control.Applicative ((<*), (<$>))

import Data.List
import Data.Char

import qualified Data.ByteString.Lazy.Char8 as BL

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString.Lazy
import ParserUtils

import Data.Accessor.Template

data Argument = NumLiteral Int 
              | StrLiteral String
              | Mask [String]
              | Labelled String Argument 
              | Object [Argument]
                deriving (Show)

data Syscall = Syscall { scName_ :: String
                       , scArgs_ :: [Argument]
                       , scRet_ :: Int
                       } deriving (Show)

$(deriveAccessors ''Syscall)

strLit :: Parser String
strLit = quoted <|> nonQuoted <?> "literal"

labelled :: Parser (String, Argument)
labelled = do
  l <- identifier
  spaces >> char '=' >> spaces
  v <- argument
  return (l, v)
  
mask :: Parser [String]  
mask = (identifier <* spaces) `sepBy1` (char '|' >> spaces)

object :: Parser [Argument]
object = do
  char '{'
  cont <- (argument <* spaces) `sepBy` (char ',' >> spaces)
  char '}'
  return cont

argument :: Parser Argument
argument = 
  (try number >>= return . NumLiteral) <|>
  (try labelled >>= return . uncurry Labelled) <|>
  (try mask >>= return . Mask) <|>
  (try strLit >>= return . StrLiteral) <|>
  (object >>= return . Object)
  
syscall :: Parser Syscall
syscall = do
  scName <- identifier
  char '(' >> spaces
  args <- argument `sepBy` (char ',' >> spaces)
  char ')' >> spaces >> char '=' >> spaces
  retc <- number
  spaces
  eof
  return $ Syscall scName args retc
  
parseSyscall :: BL.ByteString -> Either String Syscall
parseSyscall s = case parse syscall "" s of
  Left e -> Left $ show e
  Right s -> Right s