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

-- The most frequent Elglish words, we try to detect English language getting number of these frequent words at 1/3 of non-frequent words




freqengl = "the be and a of to in i you it have to that for do he with on this we that not but they say at what his from " ++
           "go or by get she my can as know if me your all who about their will so would make just up think time there see her as out one" ++
           "come  people take year him them some want how when which now like other could our into here then than look way more these no thing well because also" ++
           "two use tell good first man day find give more new one us any those very her need back there should even only many really work life why right down on" ++
           "try let something too call woman may still through mean after never no world in feel yeah great last child oh over ask when as school state much talk" ++
           "out keep leave put like help big where same all own while start three high every another become most between happen family over president old yes house" ++
           "show again student so seem might part hear its place problem where believe country always week point hand off play turn few group such"

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
      fwords = nub $ words fc
      qwords = nub $ words freqengl
      frqw = filter (`elem` qwords) fwords
      orqw = filter (not . (`elem` qwords)) fwords
      pfrqw = 100 * length frqw `div` length orqw
  let flines' = if pfrqw > 33
      then takeWhile (\l -> head (l ++ " ") /= '_') flines
      else []
  putStrLn $ fp ++ " " ++ show pfrqw
  for_ flines' $ \line -> case line of
    [] -> return ()
    '_':_ -> return ()
    _ | length (words line) > 10 -> return ()
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
        else case 1 of
          1 | numvow iw' == 0 -> return ()
          1 | nstress iw' == 0 -> return ()
          1 | nstress iw' > 8 -> return ()
          _ -> execute conn (Query $ T.pack $ "insert into " ++ tbl ++ " (line, srcid, ipa, rfd, rhyfd, numvow, stress, nstress) " ++
                                                                 "values (?,    ?,     ?,   ?,   ?,     ?,      ?,      ?)") iw'
      hFlush stdout
  return ()
