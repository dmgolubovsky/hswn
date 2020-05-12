-- Import a words index (index.adj, index.adv, index.noun, index.verb)
-- Word index entry has structure:
--
-- flavor n 3 4 @ ~ + ; 3 2 14549784 05723811 05852809
--
-- For word index import only the first two fields are of interest.
--
-- For each imported word, espeak will be invoked to determine the word's IPA,
-- number of syllables, and the stress position. These values will be also stored.


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

import qualified Data.Map as M

import IPA
import IpaMap

impWordIndex :: FilePath -> Connection -> String -> IO ()

impWordIndex fp conn tbl = do
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
          iw' = iparefine False readIPAMap iw
          newmap = mkIPAMap [iw] readIPAMap
          uncat = M.filter (== IPAUnCat) newmap
      putStrLn $ show chrs
      prtIPAMap stdout uncat
      putStrLn $ word ++ " " ++ q ++ " [" ++ concat chrs ++ "] [" ++ rfd iw' ++ "] [" ++ rhyfd iw' ++ "]"
      return ()
  return ()
