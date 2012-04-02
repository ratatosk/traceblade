{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TupleSections, TemplateHaskell #-}

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
import System.Exit

import System.Console.CmdArgs.Implicit

import Text.Parsec.ByteString.Lazy

import Data.Accessor.Template

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.ByteString.Char8 as BS

import SCParser

data CmdOpts = CmdOpts { help :: Bool
                       , rules :: String
                       } deriving (Show, Data, Typeable)

tbOpts = CmdOpts { help = def &= help "show help message" &= opt True
                 , rules = def &= help "rules used to process the trace" &= argPos 0 
                 }

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
  Nothing -> Left $ BL.pack "### No spaces found " `mappend` s
  Just 0  -> Left s
  Just x  -> let (tids, sys) = BL.splitAt x s 
                 sys' = BL.tail sys
                 tid = convertNum tids
             in case tid of
               Nothing   -> Left $ BL.pack "### Cannot parse tid:\n" `mappend` s
               Just tid' -> Right (tid' t sc) where (t, sc) = classify sys'
              
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
        Just beg' -> checkSyscall s tid $ BL.append beg' str
    Complete -> checkSyscall s tid str
    
checkSyscall :: BL.ByteString -> BL.ByteString -> Proc ()    
checkSyscall str sys = do 
  case parseSyscall sys of
    Left e -> tell [BL.pack $ "### Cannot parse syscall: " ++ e, str]
    Right s -> do
      

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
  args <- cmdArgs tbOpts
  case help args of
    True -> showHelp
    False -> run $ rules
