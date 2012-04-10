{-# LANGUAGE FlexibleContexts #-}

module ParserUtils where
 
import Data.List
import Data.Char

import Control.Applicative ((<$>))
import Control.Monad

import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

number :: Stream s m Char => ParsecT s u m Int
number = (liftM (foldl' (\z x -> 10 * z + ord x - ord '0') 0) $ many1 digit) <?> "number"

identifier :: Stream s m Char => ParsecT s u m String
identifier = (letter <|> char '_') >>= \c -> (c:) <$> many (alphaNum <|> char '_')

unesc :: Char -> Char
unesc 'n' = '\n'
unesc 't' = '\t'
unesc 'r' = '\r'
unesc 'v' = '\v'
unesc c = c

escapedChar :: Stream s m Char => ParsecT s u m Char
escapedChar = (liftM unesc $ char '\\' >> anyChar) <?> "escaped char"

doubleQuoted :: Stream s m Char => ParsecT s u m String
doubleQuoted = flip label "doublequoted literal" $ do
  _ <- char '\"'
  res <- many $ escapedChar <|> noneOf ['\n', '"']
  _ <- char '\"'
  return res
  
nonQuoted :: Stream s m Char => ParsecT s u m String
nonQuoted = (many1 $ (escapedChar <|> noneOf ", \t\n{}()")) <?> "nonquoted literal"
