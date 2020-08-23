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

main = withCliModified mods main'

-- SQL Database Path -> Syllable Pattern -> Sentence Structure

main' :: SqBase -> RhyPat -> Sentence -> Options -> IO ()

main' (SqBase sqfp) (RhyPat rhypat) (Sentence stnc) opts = do

  return ()




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
} deriving (Show, Generic, HasArguments)


mods :: [Modifier]

mods = [
  AddShortOption "rhymes" 'r'
 ,AddOptionHelp  "rhymes" "Number of rhymed lines to produce"
       ]



