{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, TemplateHaskell #-}

module Main where

import Data.Monoid
import Data.Word

import qualified Data.Accessor.Monad.MTL.State as A
import Data.Accessor.Monad.MTL.State ((%=), (%:))

import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.RWS

import System.Environment
import System.Console.GetOpt
import System.Exit

import System.IO.Unsafe

import Text.Parsec.ByteString.Lazy

import Data.Accessor.Template

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.ByteString.Char8 as BS

import SCParser

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

debug :: Show a => a -> a
debug x = unsafePerformIO $ print x >> return x

match' :: Int -> String -> Maybe Int -> Flag -> Bool
match' b _ _ (Tid a) = a == b
match' _ b _ (SCName a) = a == b
match' _ _ (Just b) (Desc a) = a == b

data ProcState = ProcState { psInput_    :: [BL.ByteString]
                           , psSkip_     :: Bool
                           , psUnfinished_ :: M.Map Int BL.ByteString
                           }
                 
$(deriveAccessors ''ProcState)

type Proc a = RWS [Flag] [BL.ByteString] ProcState a
                             
runProc :: Proc () -> [Flag] -> [BL.ByteString] -> [BL.ByteString]
runProc m f i = w where (_, w) = evalRWS m f (ProcState i False S.empty)

match :: Int -> String -> Maybe Int -> Proc Bool
match a b c = do
  f <- ask
  return $ any (match' a b c) f

data LineType = Complete | Beginning | Ending

convertNum :: BL.ByteString -> Maybe Int
convertNum s | all isDigit chars = Just $ foldl1' (\z x -> 10 * z + ord x - ord '0') chars
             | otherwise = Nothing
  where chars = BS.unpack s

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
parseLine s = case spc = BL.elemIndex ' ' s of
  Nothing -> Left $ BL.pack "?1? " `mappend` s
  Just 0  -> Left s
  Just x  -> let (tids, sys) = BL.splitAt x s 
                 sys' = BL.tail sys
                 tid = convertNum tids
             in case tid of
               Nothing   -> Left $ BL.pack "?2? " `mappend` s
               Just tid' -> Right (tid' t sc) where (t, sc) = classify sys'
              
checkLine :: BL.ByteString -> Proc ()
checkLine s | BL.null s = return ()
            | otherwise = if BL.index s 0 == ' '
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

process :: [Flag] -> IO ()
process f = do
  inLines <- BL.split '\n' <$> BL.getContents
  mapM_ BL.putStrLn (runProc mainProc f inLines)
  
main = do
  args <- getArgs
  case getOpt Permute options args of
    (f, _, []) -> process f
    (_, _, er) -> showHelp er
