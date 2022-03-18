-- Main entry point of all lyrics import programs. 
-- Lyrics files are converted into a SQLITE3 database tables
-- for faster access.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Main where

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

import ImpLyrics (impLyrics)

-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; pure $ x++xs


-- List a directory recursively. Any error in listing simply returns an empty list

listRecDir :: FilePath -> IO [FilePath]

listRecDir d = do
  lst <- try (listDirectory d)
  case lst of
    Left (_ :: IOException) -> return []
    Right ld -> do 
      sub <- concatMapM listRecDir (map (d </>) ld)
      return $ (map (d </>) ld) ++ sub

main = withCliModified mods main'

-- File to be imported -> SQL database path

main' :: ImpFile -> SqBase -> Options -> IO ()

main' (ImpFile impfp) (SqBase sqfp) opts = do
  conn <- open sqfp
  let tbl = fromMaybe (takeBaseName impfp) (table opts)
  if (not $ append opts) 
    then execute_ conn $ Query $ T.pack $ "DROP TABLE IF EXISTS " ++ tbl
    else return ()
  ld <- listRecDir impfp >>= filterM doesFileExist
  mapM (\fp -> impLyrics fp conn tbl False) ld
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
  argumentType Proxy = "path-to-lyrics-files-root"
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
  AddShortOption "table" 't'
 ,AddOptionHelp "table" "Table name"
 ,AddShortOption "append" 'a'
 ,AddOptionHelp "append" "Append to an existing table, do not replace"
       ]


