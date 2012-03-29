{-# LANGUAGE TemplateHaskell, TupleSections #-}

module SCParser where
import Control.Monad
import Control.Applicative ((<*), (<$>))

import Data.List
import Data.Char

import qualified Data.ByteString.Lazy.Char8 as BS

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString.Lazy

import Data.Accessor.Template

data Argument = NumLiteral Int 
              | StrLiteral String
              | Mask [String]
              | Labelled String Argument 
              | Object [Argument]
                deriving (Show)

data Syscall = Syscall { scName_ :: String
                       , scArgs_ :: [Argument]
                       } deriving (Show)

$(deriveAccessors ''Syscall)

number :: Parser Int
number = (liftM (foldl' (\z x -> 10 * z + ord x - ord '0') 0) $ many1 digit) <?> "number"

identifier :: Parser String
identifier = (letter <|> char '_') >>= \c -> (c:) <$> many (alphaNum <|> char '_')

unesc :: Char -> Char
unesc 'n' = '\n'
unesc 't' = '\t'
unesc 'r' = '\r'
unesc 'v' = '\v'
unesc c = c

escapedChar :: Parser Char
escapedChar = (liftM unesc $ char '\\' >> anyChar) <?> "escaped char"

quoted :: Parser String
quoted = flip label "quoted literal" $ do
  _ <- char '\"'
  res <- many $ escapedChar <|> noneOf ['\n', '"']
  _ <- char '\"'
  return res
  
nonQuoted :: Parser String
nonQuoted = (many1 $ (escapedChar <|> noneOf ", \t\n{}()")) <?> "nonquoted literal"

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
  return $ Syscall scName args