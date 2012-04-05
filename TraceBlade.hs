{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TupleSections, TemplateHaskell #-}

module Main where

import Data.Monoid
import Data.Char
import Data.List

import qualified Data.Accessor.Monad.MTL.State as A
import Data.Accessor.Monad.MTL.State ((%=), (%:))

import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.RWS

import System.Environment
import System.Exit

import System.IO
import System.Console.CmdArgs.Implicit

import Text.Parsec.ByteString.Lazy

import Data.Accessor.Template

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.ByteString.Char8 as BS

import Syscall
import Rules
import Logic

data CmdOpts = CmdOpts { rules :: String
                       } deriving (Show, Data, Typeable)

tbOpts = CmdOpts { rules = def &= argPos 0 &= typ "<rules>"
                 }

data ProcState = ProcState { psInput_      :: [BL.ByteString]
                           , psSkip_       :: Bool
                           , psUnfinished_ :: M.Map Int BL.ByteString
                           }
                 
$(deriveAccessors ''ProcState)

type Proc a = RWS Value [BL.ByteString] ProcState a
                             
runProc :: Proc () -> Value -> [BL.ByteString] -> [BL.ByteString]
runProc m f i = w where (_, w) = evalRWS m f (ProcState i False M.empty)

data LineType = Complete | Beginning | Ending deriving (Show)

convertNum :: BL.ByteString -> Maybe Int
convertNum s | all isDigit chars = Just $ foldl' (\z x -> 10 * z + ord x - ord '0') 0 chars
             | otherwise = Nothing
  where chars = BL.unpack s

classify :: BL.ByteString -> (LineType, BL.ByteString)
classify s = let (u1, u2) = BLS.breakOn unfinished s
                 (r1, r2) = BLS.breakAfter resumed s
             in case (BL.null u2, BL.null r2) of
               (False, _) -> (Beginning, u1)
               (_, False) -> (Ending, r2)
               _          -> (Complete, s)
  where
    unfinished = BS.pack "<unfinished"
    resumed = BS.pack "resumed>"

parseLine :: BL.ByteString -> Either BL.ByteString (Int, LineType, BL.ByteString)
parseLine s = case BL.elemIndex ' ' s of
  Nothing -> Left $ BL.pack "### No spaces found " `mappend` s
  Just 0  -> Left s
  Just x  -> let (tids, sys) = BL.splitAt x s 
                 sys' = BL.tail sys
                 tid = convertNum tids
             in case tid of
               Nothing   -> Left $ BL.pack "### Cannot parse tid:\n" `mappend` s
               Just tid' -> Right (tid', t, sc) where (t, sc) = classify sys'
              
checkLine :: BL.ByteString -> Proc ()
checkLine s | BL.null s = return ()
            | otherwise = case parseLine s of
  Left l -> do
    skip <- A.get psSkip
    when (not skip) $ tell [l]
  Right (tid, lt, str) -> case lt of
    Beginning -> psUnfinished %: M.insert tid str
    Ending -> do
      beg <- M.lookup tid <$> A.get psUnfinished
      case beg of
        Nothing -> tell [BL.pack "### Finish without start:", s]
        Just beg' -> do
          psUnfinished %: M.delete tid
          checkSyscall s tid $ BL.append beg' str
    Complete -> checkSyscall s tid str
    
checkSyscall :: BL.ByteString -> Int -> BL.ByteString -> Proc ()    
checkSyscall str tid sys = do 
  case parseSyscall sys of
    Left e -> tell [BL.pack $ "### Cannot parse syscall: " ++ e, sys]
    Right s -> do
      x <- ask
      case match tid s x of
        Left err -> tell [ BL.pack $ "### Error during evaluation of match expression: " ++ err
                         , BL.pack $ "### Syscall was: " ++ show s]
        Right True -> do 
          tell [str]
          psSkip %= False
        Right False -> psSkip %= True

nextLine :: Proc (Maybe BL.ByteString)
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

process :: Value -> IO ()
process x = do
  inLines <- BL.split '\n' <$> BL.getContents
  mapM_ (\x -> BL.putStrLn x >> hFlush stdout) (runProc mainProc x inLines)
  
main = do
  opts <- cmdArgs tbOpts
  case parseRules (rules opts) of
    Left err -> do
      putStrLn $ "Cannot parse matching rules: " ++ err
      exitWith $ ExitFailure 1
    Right expr -> process expr