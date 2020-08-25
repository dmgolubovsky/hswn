-- WN rhymer, 
-- builds a given number of rhyming lines by the desired
-- syllable stress pattern, and sentence structure.


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
import Data.List (nub, isSuffixOf, take, partition)
import Control.Monad.HT (for)
import Control.Monad.Loops
import System.FilePath
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import qualified Data.Text as T

data IPAData = IPAData {
  word :: String                     -- word proper
 ,qual :: String                     -- part of speech
 ,ipa :: String                      -- original transcription
 ,rfd :: String                      -- refined IPA (expressed in chars)
 ,rhyfd :: String                    -- refined IPA for rhyming (consonants condensed)
 ,numvow :: Int                      -- number of vowels
 ,stress :: Int                      -- stressed vowel position from end
} deriving (Ord, Eq, Show)

instance FromRow IPAData where fromRow = IPAData <$> field <*> field <*> field <*> field <*> field <*> field <*> field

main = withCliModified mods main'

-- SQL Database Path -> Syllable Pattern -> Sentence Structure

main' :: SqBase -> RhyPat -> Sentence -> Options -> IO ()

main' (SqBase sqfp) (RhyPat rhypat) (Sentence stnc) opts = do
  let nrhy = fromMaybe 10 (rhymes opts)
  conn <- open sqfp
  rword <- case (endw opts) of
             Just w -> return w
             Nothing -> getRandomWord conn rhypat
  putStrLn rword
  rhymes <- allRhymes conn rword nrhy rhypat stnc
  close conn
  return ()

-- Find last stress position in a pattern. This is the position of the first
-- capital letter from the end, and if no capital letter is found then
-- result is 0 (last syllable)

findPatStress :: String -> Int

findPatStress pat = fps 0 (reverse pat) where
  fps p [] = p
  fps p (c:cs) | isUpper c = p
  fps p (c:cs) = fps (p + 1) cs

-- Get a desired number of rhymes for the given word with respect to
-- part of speech and rhytmic patern (last stressed syllable position

allRhymes :: Connection -> String -> Int -> String -> String -> IO [String]

allRhymes conn rword nrhy rhypat stnc = do
  let rq = toLower $ head (reverse stnc)
  res <- queryNamed conn [sql|select distinct * from 
                                (select * from adverb union 
                                 select * from adject union 
                                 select * from noun union 
                                 select * from verb) 
                                   where word = :word|] [":word" := map toLower rword] :: IO [IPAData]
  case res of
    [] -> error $ "cannot retrieve information for " ++ rword
    (rw:_) -> do
      putStrLn $ show rw
      let patstr = findPatStress rhypat
      putStrLn $ "pattern stress pos " ++ show patstr
      if stress rw /= patstr
        then putStrLn $ "warning: pattern stress =/= word stress"
        else return ()
      let h1 = nrhy `div` 2
          h2 = nrhy - h1
      let getwords cmp dir nw = query_ conn (Query $ T.pack $ 
           "select distinct * from " ++
              "(select * from adverb union " ++
              "select * from adject union " ++
              "select * from noun union " ++
              " select * from verb) " ++
                " where rhyfd " ++ cmp ++ "'" ++ rhyfd rw ++ "' " ++
                " and stress = " ++ show patstr ++ 
                " and numvow <= " ++ show (numvow rw) ++
                " and word not like '% %' " ++
                " and qual = '" ++ [rq] ++ "' " ++
                  " order by rhyfd " ++ dir ++ " limit " ++ show (if nw < 0 then 0 else nw)) :: IO [IPAData]
      rhys1 <- getwords ">" "asc" h1
      rhys2 <- getwords "<" "desc" h2
      mapM (putStrLn . show . word) (rhys1 ++ rhys2)
  return []


-- Get a random word from the entire database wrt the desired rhyming pattern

getRandomWord :: Connection -> String -> IO String

getRandomWord conn rhypat = do
  let patstr = findPatStress rhypat
  res <- queryNamed conn [sql|select distinct word, qual from 
                             (select * from adverb union 
                              select * from adject union 
                              select * from noun union 
                              select * from verb) 
                                where word not like '% %' and stress = :stress
                              order by random() limit 1|] [":stress" := patstr] :: IO [(String, String)]
  case res of
    [] -> error "cannot make random selection"
    x  -> return $ fst (head res)


data SqBase = SqBase FilePath

instance Argument SqBase where
  argumentType Proxy = "path-to-the-sqlite-database"
  parseArgument f = Just (SqBase f)

instance HasArguments SqBase where
  argumentsParser = atomicArgumentsParser


data RhyPat = RhyPat String

instance Argument RhyPat where
  argumentType Proxy = "rhyme-pattern"
  parseArgument f = Just (RhyPat f)

instance HasArguments RhyPat where
  argumentsParser = atomicArgumentsParser

data Sentence = Sentence String

instance Argument Sentence where
  argumentType Proxy = "sentence-structure"
  parseArgument f = Just (Sentence f)

instance HasArguments Sentence where
  argumentsParser = atomicArgumentsParser


data Options = Options {
  rhymes :: Maybe Int
 ,endw :: Maybe String
} deriving (Show, Generic, HasArguments)


mods :: [Modifier]

mods = [
  AddShortOption "rhymes" 'r'
 ,AddOptionHelp  "rhymes" "Number of rhymed lines to produce, default is 10"
 ,AddShortOption "endw" 'w'
 ,AddOptionHelp  "endw" "Ending word of the line"
       ]



