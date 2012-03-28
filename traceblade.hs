{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, TemplateHaskell #-}

module Main where

import Data.Word

import qualified Data.Accessor.Monad.MTL.State as A
import Data.Accessor.Monad.MTL.State ((%=), (%:))

import qualified Data.Set as S

import Control.Applicative
import Control.Monad
import Control.Monad.RWS

import System.Environment
import System.Console.GetOpt
import System.Exit

import System.IO.Unsafe

import Text.Parsec.ByteString.Lazy

import Data.Accessor.Template

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Search as BSS

data Flag = Help | Tid Int | SCName String | Desc Int

options :: [OptDescr Flag]
options = 
  [ Option ['h'] ["help"]    (NoArg Help) "show help message"
  , Option ['t'] ["tid"]     (ReqArg (Tid . read) "TID") "filter by thread id"
  , Option ['s'] ["syscall"] (ReqArg SCName "SYSCALL") "filter by syscall name"
  , Option ['d'] ["descr"]   (ReqArg (Desc . read) "FD") "filter by file descriptor"
  ]
  
data Syscall = Syscall String (Maybe Int)

data Action = Begin Syscall | End

type Line = (Int, Action)

showHelp :: [String] -> IO a
showHelp errs = do 
  putStrLn $ concat errs ++ usageInfo "Usage: traceblade [OPTION...]" options
  exitWith $ if null errs then ExitSuccess else ExitFailure 1

contains :: String -> BS.ByteString -> Bool
contains pat = not . null . BSS.indices (BSS.strictify $ BS.pack pat)

pInt :: Parser Int
pInt = foldl' (\z x -> 10 * z + ord x - ord '0') 0 <$> many1 digit

pSCName :: Parser String
pSCName = many1 (letter <|> digit <|> char '_')

pAction :: Parser Action
pAction = pEnd <|> pBegin

pEnd :: Parser Action
pEnd = string "<..."


pLine :: Parser Line
pLine = do
  tid <- pInt
  spaces
  act <- pAction
  return (tid, act)

isResumed :: BS.ByteString -> Bool
isResumed = contains "resumed>"

extractFD :: BS.ByteString -> Maybe Int
extractFD s = Nothing

extractSC :: BS.ByteString -> String
extractSC = BS.unpack . BS.takeWhile (/= '(')

parseSC :: BS.ByteString -> Action
parseSC s | isResumed s = End
          | otherwise = Begin $ Syscall (extractSC s) (extractFD s)

debug :: Show a => a -> a
debug x = unsafePerformIO $ print x >> return x

cutBy :: Char -> BS.ByteString -> (BS.ByteString, BS.ByteString)
cutBy c s = case c `BS.elemIndex` s of 
  Just idx -> 

parse :: BS.ByteString -> Line
parse l = (tid, parseSC rem)
  where 
    (tids, rem) = BS.span (/= ' ') l
    tid = read $ BS.unpack tids

match' :: Int -> String -> Maybe Int -> Flag -> Bool
match' b _ _ (Tid a) = a == b
match' _ b _ (SCName a) = a == b
match' _ _ (Just b) (Desc a) = a == b

data ProcState = ProcState { psInput_    :: [BS.ByteString]
                           , psSkip_     :: Bool
                           , psContTids_ :: S.Set Int
                           }
                 
$(deriveAccessors ''ProcState)

type Proc a = RWS [Flag] [BS.ByteString] ProcState a
                             
runProc :: Proc () -> [Flag] -> [BS.ByteString] -> [BS.ByteString]
runProc m f i = w where (_, w) = evalRWS m f (ProcState i False S.empty)

match :: Int -> String -> Maybe Int -> Proc Bool
match a b c = do
  f <- ask
  return $ any (match' a b c) f

checkLine :: BS.ByteString -> Proc ()
checkLine s | BS.null s = return ()
            | otherwise = if BS.index s 0 == ' '
              then runDumpLine s
              else runSCLine (parse s)
  where
    runDumpLine s = do
      need <- A.get psSkip
      when need $ tell [s]
    runSCLine (tid, act) = do
      case act of
        Begin (Syscall n f) -> do
          need <- match tid n Nothing
          psSkip %= need
          psContTids %: if need then S.insert tid else S.delete tid
          when need $ tell [s]
        End -> do
          need <- S.member tid <$> A.get psContTids
          psSkip %= need
          when need $ tell [s]

nextLine :: Proc (Maybe BS.ByteString)
nextLine = do
  lines <- A.get psInput
  case lines of
    []     -> return Nothing
    (x:xs) -> do 
      psInput %: tail
      return $ Just x
      
      

mainProc :: Proc ()
mainProc = do
  l <- nextLine
  case l of
    Just l -> do
      checkLine l
      mainProc
    Nothing -> return ()

process :: [Flag] -> IO ()
process f = do
  inLines <- BS.split '\n' <$> BS.getContents
  mapM_ BS.putStrLn (runProc mainProc f inLines)
  
main = do
  args <- getArgs
  case getOpt Permute options args of
    (f, _, []) -> process f
    (_, _, er) -> showHelp er
