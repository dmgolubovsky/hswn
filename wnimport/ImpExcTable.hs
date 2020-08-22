-- Import an exception table (adj.exc, adv.exc, noun.exc, verb.exc)
-- Exception table has structure:
--
-- altered-form proper-form                            
--
-- Exception tables are used to find the assumed proper form to look up in the
-- dictionary.


{-# LANGUAGE OverloadedStrings #-}

module ImpExcTable (impExcTable) where

import qualified System.IO.Strict as SIO
import System.Process
import System.Environment
import Data.List.Split
import System.IO
import Data.Word
import Data.Char
import Data.List
import Data.Bool
import Data.Maybe
import Data.Foldable
import System.FilePath
import Data.List.HT (replace)
import Database.SQLite.Simple

import qualified Data.Text as T
import qualified Data.Map as M

impExcTable :: FilePath -> Connection -> String -> IO ()

impExcTable fp conn tbl = do
  execute_ conn $ Query $ T.pack $ "create table if not exists " ++ tbl ++ "(" ++
                                   "altered TEXT, " ++
                                   "proper TEXT " ++ ")"
  flines <- readFile fp >>= return . lines
  for_ flines $ \line -> case line of
    [] -> return ()
    ' ':_ -> return ()
    _ -> do
      let [altered, proper] = take 2 $ words line
      putStrLn ("Importing " ++ altered ++ " " ++ proper)
      execute conn (Query $ T.pack $ "insert into " ++ tbl ++ " (altered, proper) " ++
                                                        "values (?,    ?)") (altered, proper)
      hFlush stdout
  return ()

