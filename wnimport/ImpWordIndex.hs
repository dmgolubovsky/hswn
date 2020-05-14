-- Import a words index (index.adj, index.adv, index.noun, index.verb)
-- Word index entry has structure:
--
-- flavor n 3 4 @ ~ + ; 3 2 14549784 05723811 05852809
--
-- For word index import only the first two fields are of interest.
--
-- For each imported word, espeak will be invoked to determine the word's IPA,
-- number of syllables, and the stress position. These values will be also stored.

{-# LANGUAGE OverloadedStrings #-}

module ImpWordIndex (impWordIndex) where

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

import IPA
import IpaMap

instance ToRow IPAWord where
  toRow iw = toRow (word iw, qual iw, concat $ map fromIPA $ ipa iw, rfd iw, rhyfd iw, numvow iw, stress iw)

impWordIndex :: FilePath -> Connection -> String -> Bool -> IO ()

impWordIndex fp conn tbl ustop = do
  execute_ conn $ Query $ T.pack $ "create table if not exists " ++ tbl ++ "(" ++
                                   "word TEXT, " ++
                                   "qual TEXT, " ++
                                   "ipa  TEXT, " ++
                                   "rfd  TEXT, " ++
                                   "rhyfd TEXT, " ++
                                   "numvow INTEGER, " ++
                                   "stress INTEGER" ++ ")"
  flines <- readFile fp >>= return . lines
  for_ flines $ \line -> case line of
    [] -> return ()
    ' ':_ -> return ()
    _ -> do
      let [word', q] = take 2 $ words line
          word = replace "_" " " word'
      ipas <- getIPA word
      let chrs = map fromIPA ipas
          iw = ipaword word ipas
          iw' = iparefine False readIPAMap iw {qual = q}
          newmap = mkIPAMap [iw] readIPAMap
          uncat = M.filter (== IPAUnCat) newmap
      if M.size uncat > 0 && ustop
        then do
          prtIPAMap stdout uncat
          putStrLn $ word ++ " " ++ q ++ " [" ++ concat chrs ++ "] [" ++ rfd iw' ++ "] [" ++ rhyfd iw' ++ "] (" ++ 
                     show (M.size uncat) ++ ")"
        else do
          putStr $ "Importing " ++ word ++ " " ++ q ++ " [" ++ concat chrs ++ "]\r"
          execute conn (Query $ T.pack $ "insert into " ++ tbl ++ " (word, qual, ipa, rfd, rhyfd, numvow, stress) " ++
                                                            "values (?,    ?,    ?,   ?,   ?,     ?,      ?)") iw'
          hFlush stdout
  return ()
