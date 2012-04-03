{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleContexts #-}

module Syscall ( Syscall(..)
               , scName
               , scArgs
               , scRet
               , Argument(..)
               , parseSyscall
               ) where

import Control.Monad
import Control.Applicative ((<*), (<$>), (<*>))

import Data.List
import Data.Char

import qualified Data.ByteString.Lazy.Char8 as BL

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString.Lazy
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as T

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
                       , scErrno_ :: Maybe (String, String)
                       } deriving (Show)

$(deriveAccessors ''Syscall)

def :: Stream s m Char => T.GenTokenParser s u m
def = T.makeTokenParser $ T.emptyDef { T.identStart = letter
                                     , T.identLetter = alphaNum <|> char '_'
                                     , T.opStart = oneOf "=.|"
                                     , T.opLetter = oneOf "=.|"
                                     , T.reservedOpNames = ["=", "...", "|"]
                                     }

tOp :: String -> Parser ()
tOp = T.reservedOp def

tString :: Parser String
tString = T.stringLiteral def <* optional (tOp "...")

tId :: Parser String
tId = T.identifier def <|> (T.lexeme def $ string "@")

tNum :: Parser Int
tNum = fromIntegral <$> T.integer def

labelled :: Parser (String, Argument)
labelled = do
  l <- tId
  tOp "="
  v <- argument
  return (l, v)
  
mask :: Parser [String]  
mask = tId `sepBy1` tOp "|"

object :: Parser [Argument]
object = T.braces def cont <|> T.brackets def cont
  where
    cont = T.commaSep def argument

argument :: Parser Argument
argument = 
  (try tNum >>= return . NumLiteral) <|>
  (try labelled >>= return . uncurry Labelled) <|>
  (try mask >>= return . Mask) <|>
  (try tString >>= return . StrLiteral) <|>
  (object >>= return . Object)

errnoStr :: Parser String
errnoStr = T.lexeme def $ do
  char '('
  content <- many1 $ noneOf ")"
  char ')'
  return content

syscall :: Parser Syscall
syscall = do
  spaces
  scName <- tId
  args <- T.parens def $ T.commaSep def argument
  tOp "="
  retc <- tNum
  errno <- optionMaybe $ (,) <$> tId <*> errnoStr
  eof
  return $ Syscall scName args retc errno
  
parseSyscall :: BL.ByteString -> Either String Syscall
parseSyscall s = case parse syscall "" s of
  Left e -> Left $ show e
  Right s -> Right s