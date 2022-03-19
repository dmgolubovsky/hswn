-- Import song lyrics line by line from a file
--
-- For each file its hash will be computed to use as an unique source id
--
-- For each imported line, espeak will be invoked to determine the line's IPA,
-- number of syllables, and the stress position. These values will be also stored.

{-# LANGUAGE OverloadedStrings #-}

module ImpLyrics (impLyrics) where

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

import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5


import IPA
import IpaMap

instance ToRow IPALine where
  toRow iw = toRow (line iw, srcid iw, concat $ map fromIPA $ ipa iw, rfd iw, rhyfd iw, numvow iw, stress iw, nstress iw)

-- The most frequent Elglish words, we try to detect English language to get more than half of these words occurring



freqengl = "the be and a of to in i you it have to that for do he with on this we that not but they say at what his from " ++
           "go or by get she my can as know if me your all who about their will so would make just up think time there see her as out one"

impLyrics :: FilePath -> Connection -> String -> Bool -> IO ()

impLyrics fp conn tbl ustop = do
  execute_ conn $ Query $ T.pack $ "create table if not exists " ++ tbl ++ "(" ++
                                   "line TEXT, " ++
                                   "srcid TEXT, " ++
                                   "ipa  TEXT, " ++
                                   "rfd  TEXT, " ++
                                   "rhyfd TEXT, " ++
                                   "numvow INTEGER, " ++
                                   "stress INTEGER, " ++ 
                                   "nstress INTEGER" ++ ")"
  fileContent <- LB.readFile fp
  let md5Digest = md5 fileContent
  fc <- readFile fp
  let flines = lines fc
      fwords = words fc
      qwords = words freqengl
      frqw = nub $ filter (`elem` qwords) fwords
      pfrqw = 100 * length frqw `div` length qwords
  let flines' = if pfrqw > 50
      then takeWhile (\l -> head (l ++ " ") /= '_') flines
      else []
  for_ flines' $ \line -> case line of
    [] -> return ()
    '_':_ -> return ()
    _ -> do
      let line' = replace "_" " " line
      ipas <- getIPA line'
      let chrs = map fromIPA ipas
          iw = (ipaline line ipas) {srcid = show md5Digest}
          iw' = iparefine True readIPAMap iw
          newmap = mkIPAMap [iw] readIPAMap
          uncat = M.filter (== IPAUnCat) newmap
      if M.size uncat > 0 && ustop
        then do
          prtIPAMap stdout uncat
          putStrLn $ line ++ " " ++ " [" ++ concat chrs ++ "] [" ++ rfd iw' ++ "] [" ++ rhyfd iw' ++ "] (" ++ 
                     show (M.size uncat) ++ ")"
        else do
          putStrLn $ "Importing " ++ line ++ " [" ++ concat chrs ++ "]"
          execute conn (Query $ T.pack $ "insert into " ++ tbl ++ " (line, srcid, ipa, rfd, rhyfd, numvow, stress, nstress) " ++
                                                            "values (?,    ?,     ?,   ?,   ?,     ?,      ?,      ?)") iw'
          hFlush stdout
  return ()
