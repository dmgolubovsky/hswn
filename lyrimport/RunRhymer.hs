-- Run the rhymer receiving the text on standard input and appending rhymes
-- to the standard output.
--
-- A lyrics file consists of lines of text some with certain remarks.
--
-- If a line starts with a # it is a comment, and the line is ignored.
-- If a line (or multiple lines) start with a * then the last line marked this way
-- will be used for rhyming. If no lines are marked, the very last line not containing
-- a rhyming request will be used.
-- If a line starts with a / then it is a rhyming request. Via rhyming request,
-- desired number of vowels, stressed vowels, and last stress position may be altered,
-- otherwise they are taken from the line marked for rhyming.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}


module RunRhymer (runRhymer) where

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
import Database.SQLite.Simple.QQ

import qualified Data.Text as T
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5


import IPA
import IpaMap

data LyrLine = 
   LyrLine IPALine             -- a regular line
 | RhyLine IPALine             -- a line to find rhyme to
 | CommLine String             -- a comment, to be ignored
   deriving (Show)

instance ToRow IPALine where
  toRow iw = toRow (line iw, srcid iw, concat $ map fromIPA $ ipa iw, rfd iw, rhyfd iw, numvow iw, stress iw, nstress iw)

-- Retrievable IPA line

data IPALineR = IPALineR {
  lineR :: String                    -- line proper
 ,srcidR :: String                   -- source ID (hash of a lyrics file)
 ,ipacR :: String                    -- IPA (expressed in UTF chars)
 ,rfdR :: String                     -- refined IPA (expressed in chars)
 ,rhyfdR :: String                   -- refined IPA for rhyming (consonants condensed)
 ,numvowR :: Int                     -- number of vowels
 ,stressR :: Int                     -- stressed vowel position from end
 ,nstressR :: Int                    -- number of stressed syllables
} deriving (Ord, Eq, Show)
  

instance FromRow IPALineR where fromRow = IPALineR <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field


mkIPALine :: String -> IO IPALine

mkIPALine line = do
  ipas <- getIPA line
  let iw = (ipaline line ipas) {srcid = ""}
  return $ iparefine True readIPAMap iw

mkLyrLine :: String -> IO LyrLine

mkLyrLine ('#':rl) = return $ CommLine rl

mkLyrLine ('*':rl) = mkIPALine rl >>= return . RhyLine

mkLyrLine l = mkIPALine l >>= return . LyrLine

simpleRhyme :: Connection -> String -> IPALine -> IO [String]

simpleRhyme conn tbl il = do
  execute_ conn (Query $ T.pack ("create temporary view lyrrhymer_table as select * from " ++ tbl ++ "; "))
  rhy1 <- queryNamed conn [sql|select distinct * 
                                 from lyrrhymer_table 
                                   where rhyfd > :rfd and 
                                         nstress = :ns and 
                                         stress = :st and
                                         srcid not in (select srcid from srcids)
                                     order by rhyfd limit 3|]
                          [":rfd" := rhyfd il, ":ns" := nstress il, ":st" := stress il] :: IO [IPALineR]
  rhy2 <- queryNamed conn [sql|select distinct * 
                                 from lyrrhymer_table 
                                   where rhyfd < :rfd and 
                                         nstress = :ns and 
                                         stress = :st and
                                         srcid not in (select srcid from srcids)
                                     order by rhyfd desc limit 3|]
                          [":rfd" := rhyfd il, ":ns" := nstress il, ":st" := stress il] :: IO [IPALineR]
  return $ map lineR (rhy2 ++ rhy1)

findRhymeFor :: [LyrLine] -> Maybe IPALine

findRhymeFor ll = let
    isRhyLine (RhyLine _) = True
    isRhyLine _ = False
    isLyrLine (LyrLine _) = True
    isLyrLine _ = False
    rhls = filter isRhyLine ll
    llls = filter isLyrLine ll
    lastLine = case llls of
      [] -> []
      _ -> case head llls of
        LyrLine l -> [RhyLine l]
        _ -> []
    lls = rhls ++ lastLine ++ [CommLine "Nothing to rhyme"]
  in case head lls of
       RhyLine il -> Just il
       _ -> Nothing

collectSources :: Connection -> String -> [LyrLine] -> IO ()

collectSources conn tbl ll = do
  execute_ conn (Query $ T.pack "create temporary table srcids (srcid TEXT)")
  forM_ ll $ \lyr -> do
    let xline = case lyr of
                  LyrLine il -> line il
                  CommLine st -> st
                  RhyLine il -> line il
    execute_ conn (Query $ T.pack ("insert into srcids (SRCID) select SRCID from " ++ tbl ++ " where LINE = \"" ++ xline ++ "\""))
  return ()


runRhymer :: Connection -> String -> IO ()

runRhymer conn tbl = do
  fc <- readFile "/dev/stdin"
  let flines = filter ((/=0) . length) $ lines fc
  lyrlines <- mapM mkLyrLine flines >>= return . reverse
  collectSources conn tbl lyrlines
  let rl = findRhymeFor lyrlines
  case rl of
    Just il -> do
      sr <- simpleRhyme conn tbl il
      mapM_ putStrLn flines
      mapM_ putStrLn (map ('#':) sr)
    Nothing -> mapM_ putStrLn flines
  return ()

