-- Main entry point of all lyrics import programs. 
-- Lyrics files are converted into a SQLITE3 database tables
-- for faster access.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module RhyMain where

-- Package modules

import WithCli
import Data.Maybe
import System.IO
import Control.Monad
import System.FilePath
import System.Directory
import Control.Exception
import Database.SQLite.Simple
import qualified Data.Text as T

-- Local modules

import RunRhymer (runRhymer)

main = withCliModified mods main'

-- SQL database path

main' :: SqBase -> Options -> IO ()

main' (SqBase sqfp) opts = do
  conn <- open sqfp
  let tbl = fromMaybe "database" (table opts)
  runRhymer conn tbl
  close conn
  return ()

data SqBase = SqBase FilePath

instance Argument SqBase where
  argumentType Proxy = "path-to-the-sqlite-database"
  parseArgument f = Just (SqBase f)

instance HasArguments SqBase where
  argumentsParser = atomicArgumentsParser


data Options = Options {
  table :: Maybe String
} deriving (Show, Generic, HasArguments)

mods :: [Modifier]

mods = [
  AddShortOption "table" 't'
 ,AddOptionHelp "table" "Table name"
       ]


