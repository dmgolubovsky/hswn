-- Main entry point of all WN import programs. 
-- WN files are converted into a SQLITE3 database tables
-- for faster access.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Main where

-- Package modules

import WithCli
import Data.Maybe
import System.IO
import System.FilePath
import Database.SQLite.Simple
import qualified Data.Text as T

-- Local modules

import ImpWordIndex (impWordIndex)

main = withCliModified mods main'

-- File to be imported -> SQL database path

main' :: ImpFile -> SqBase -> Options -> IO ()

main' (ImpFile impfp) (SqBase sqfp) opts = do
  conn <- open sqfp
  let tbl = fromMaybe (takeBaseName impfp) (table opts)
  if (not $ append opts) 
    then execute_ conn $ Query $ T.pack $ "DROP TABLE IF EXISTS " ++ tbl
    else return ()
  case (mode opts) of
    Just "index" -> impWordIndex impfp conn tbl
    _ -> hPutStrLn stderr "Import mode unsupported or unspecified"
  close conn
  return ()

data SqBase = SqBase FilePath

instance Argument SqBase where
  argumentType Proxy = "path-to-the-sqlite-database"
  parseArgument f = Just (SqBase f)

instance HasArguments SqBase where
  argumentsParser = atomicArgumentsParser


data ImpFile = ImpFile FilePath

instance Argument ImpFile where
  argumentType Proxy = "path-to-a-wn-file-to-be-imported"
  parseArgument f = Just (ImpFile f)

instance HasArguments ImpFile where
  argumentsParser = atomicArgumentsParser


data Options = Options {
  mode :: Maybe String
 ,table :: Maybe String
 ,append :: Bool
} deriving (Show, Generic, HasArguments)

mods :: [Modifier]

mods = [
  AddShortOption "mode" 'm'
 ,AddOptionHelp "mode" "Import mode"
 ,AddShortOption "table" 't'
 ,AddOptionHelp "table" "Table name"
 ,AddShortOption "append" 'a'
 ,AddOptionHelp "append" "Append to an existing table, do not replace"
       ]


