module RuleParser ( Rules(..)
                  , Param(..)
                  , Op(..)
                  , Value(..)
                  , parseRules
                  ) where

import Control.Applicative ((<$>), (<*>), (<*))

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.String

data Param = Tid | SyscallName | FileDesc | RetCode deriving (Show)
data Op = Equals | Contains deriving (Show)
data Value = Value String deriving (Show)
data Rules = Assertion Param Op Value | And Rules Rules | Or Rules Rules deriving (Show)

parens :: Parser a -> Parser a
parens x = flip label "expression in parens" $ do
  sChar '('
  r <- x
  sChar ')'
  return r

sChar :: Char -> Parser ()
sChar c = char c >> spaces

param :: Parser Param
param = (sChar 't' >> return Tid) <|>
        (sChar 's' >> return SyscallName) <|>
        (sChar 'f' >> return FileDesc) <|>
        (sChar 'r' >> return RetCode)

op :: Parser Op
op = (sChar '=' >> return Equals) <|>
     (sChar '~' >> return Contains)
     
value :: Parser Value
value = (Value <$> many1 alphaNum) <* spaces

assertion :: Parser Rules
assertion = Assertion <$> param <*> op <*> value

rules :: Parser Rules
rules = spaces >> rules' <* eof 
  where
    rules' = buildExpressionParser table term
    term = parens rules' <|> assertion
    table = [ [binary '&' And]
            , [binary '|' Or]
            ]
    binary name func = Infix (sChar name >> return func) AssocLeft

parseRules :: String -> Either String Rules
parseRules s = case parse rules "" s of
  Left e -> Left $ show e
  Right s -> Right s