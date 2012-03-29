{-# LANGUAGE TemplateHaskell, TupleSections #-}

module SCParser where

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.ByteString.Lazy

import Data.Accessor.Template

type Argument = NumLiteral Int | StrLiteral String | Labelled String ArgData | Object [ArgData]

data Syscall = Syscall { scName_ :: String
                       , scArgs_ :: [ScArt
                       } deriving (Show)

data LineType = Start | End | Complete

data Line = Line { lTid_     :: Int
                 , lType_    :: LineType
                 , lSyscall_ :: Syscall
                 } deriving (Show)
                            
$(deriveAccessors ''Syscall)
$(deriveAccessors ''Line)

str2int :: String -> Maybe Int
str2int = foldl' (\z x -> 10 * z + ord x - ord '0') 0 <$> many1 digit

line :: Parser Line
line = do
  tid <- maybe (fail "Cannot parse number") id . str2int <$> many1 digit
  spaces
  (lt, sc) <- syscall
  return $ Line tid sType sc
  
syscall :: Parser (LineType, Syscall) 
syscall = 
  (try start >>= return . (Start,)) <|>
  (try end >>= return . (End,)) <|>
  (complete >>= return . (Complete,))

identifier :: Parser String
identifier = many1 $ alphaNum <|> char '_'

unesc :: Char -> Char
unesc 'n' = '\n'
unesc 't' = '\t'
unesc 'r' = '\r'
unesc 'v' = '\v'
unesc c = c

escapedChar :: IOParser Char
escapedChar = (liftM unesc $ char '\\' >> anyChar) <?> "escaped char"

quoted :: Parser String
quoted = flip label "quoted literal" $ do
  _ <- char '\"'
  res <- many $ escapedChar <|> noneOf ['\n', '"']
  _ <- char '\"'
  return res
  
nonQuoted :: Parser String
nonQuoted = (many1 $ (escapedChar <|> noneOf [',', ' ', '\t', '\n', '#'])) <?> "nonquoted literal"

strLiteral :: IOParser String
strLiteral = quoted <|> nonQuoted <?> "literal"

argument :: Parser Argument

getStart :: Parser Syscall
getStart = do
  scName <- identifier
  char '(' >> spaces
  args <- argument `sepBy` (char ',' >> spaces)
  
  
  

