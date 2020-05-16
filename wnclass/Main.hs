-- WN word classifier. Receives words on stdin, 
-- prints words on stdout with their class (part of speech)


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import WithCli
import Data.Maybe
import System.IO
import Data.Char
import Data.List (nub)
import Control.Monad.HT (for)
import System.FilePath
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import qualified Data.Text as T

data WordClass = WordClass {
  root :: String         -- part of word that matched
 ,qual :: String         -- part of speech detected (empty string if not detected)
 ,suffix :: String       -- suffix that was removed to match
} deriving (Show)


unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs

classWord :: Connection -> String -> IO [WordClass]

classWord conn w = do
  res <- queryNamed conn [sql|select word, qual from 
                                (select * from adverb union 
                                 select * from adject union 
                                 select * from noun union 
                                 select * from verb) 
                                   where word = :word|] [":word" := w] :: IO [(String, String)]
  case res of
    [] -> return [WordClass w "" ""]
    rs@((r, q):_) -> return $ unzipWith wc rs where wc a b = WordClass a b ""

main = withCliModified mods main'

main' :: SqBase -> Options -> IO ()

main' (SqBase sqfp) opts = do
  w0 <- readFile "/dev/stdin"
  let wds = nub $ words $ map toLower w0
  conn <- open sqfp
  for wds $ \w -> do
    res <- classWord conn w
    mapM (putStrLn . show) res
  close conn
  return ()


data SqBase = SqBase FilePath

instance Argument SqBase where
  argumentType Proxy = "path-to-the-sqlite-database"
  parseArgument f = Just (SqBase f)

instance HasArguments SqBase where
  argumentsParser = atomicArgumentsParser

data Options = Options {
  dummy :: Bool          
} deriving (Show, Generic, HasArguments)

mods = [
       ]

