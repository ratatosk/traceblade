module Rules ( Value(..)
             , parseRules
             ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import ParserUtils

data Value = B Bool | A String | N Int | S String | L [Value] 
          deriving (Eq, Show)

expr :: Parser Value
expr = (A <$> identifier) <|> 
       (N <$> number) <|>
       (S <$> quoted) <|>
       (L <$> list)

list :: Parser [Value]
list = (char '(' >> spaces) *> manyTill (expr <* spaces) (char ')')

parseRules :: String -> Either String Value
parseRules s = case parse expr "" s of
  Left e -> Left $ show e
  Right s -> Right s