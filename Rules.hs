module Rules ( Expr(..)
             , parseRules
             ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Text.Parsec.String
import ParserUtils

data Expr = BoolAtom Bool | IdAtom String | NumAtom Int | StrAtom String | List [Expr] 
          deriving (Eq, Show)

expr :: Parser Expr
expr = (IdAtom <$> identifier) <|> 
       (NumAtom <$> number) <|>
       (StrAtom <$> quoted) <|>
       (List <$> list)

list :: Parser [Expr]
list = (char '(' >> spaces) *> manyTill (expr <* spaces) (char ')')

parseRules :: String -> Either String Expr
parseRules s = case parse expr "" s of
  Left e -> Left $ show e
  Right s -> Right s