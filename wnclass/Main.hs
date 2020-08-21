-- WN word classifier. Receives words on stdin, 
-- prints words on stdout with their class (part of speech)


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

data WordClass = WordClass {
  root :: String         -- part of word that matched
 ,qual :: String         -- part of speech detected (empty string if not detected)
 ,suffix :: String       -- suffix that was removed to match
} deriving (Eq, Show)


unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs

classWord :: Connection -> String -> IO [WordClass]

classWord conn w = do
  res <- queryNamed conn [sql|select word, qual from 
                                (select * from adverb union 
                                 select * from adject union 
                                 select * from noun union 
                                 select * from verb) 
                                   where word = :word|] [":word" := w] :: IO [(String, String)]
  case res of
    [] -> return [WordClass w "" ""]
    rs@((r, q):_) -> return $ unzipWith wc rs where wc a b = WordClass a b ""


suffixes = ["", "s", "'s", "es", "d", "ed", "ing"]

stripSuffix :: String -> [(String, String)]

stripSuffix w = catMaybes $ map (onesfx w) suffixes where
  onesfx w "" = Just (w, "")
  onesfx w s | s `isSuffixOf` w = Just (take ln w, s) where ln = length w - length s
  onesfx _ _ = Nothing

main = withCliModified mods main'

main' :: SqBase -> Options -> IO ()

main' (SqBase sqfp) opts = do
  w0 <- readFile "/dev/stdin"
  let wds = words $ map toLower w0
  conn <- open sqfp
  for wds $ \w -> do
    let ssfx = stripSuffix w
    res' <- for ssfx $ \(wd, sx) -> do
      let hasQual (WordClass _ "" "") = True
          hasQual (WordClass _ q _) | length q > 0 = True
          hasQual (WordClass _ "" s) | length s > 0 = False
          hasQual (WordClass _ _ _) = False
      rr <- classWord conn wd >>= return . nub
      rr' <- for rr (\(WordClass r q _) -> return $ WordClass r q sx)
      return $ filter hasQual rr'
    let res = concat res'
    let partGrp s = case partition (\(WordClass _ q _) -> length q == 0) s of
          ([], []) -> []
          (s1, []) -> take 1 s1
          (_, s2) -> s2
    let pres = partGrp res
    putStrLn $ showRes pres
  close conn
  return ()

showRes :: [WordClass] -> String

showRes [] = "<Nothing>"
showRes ((WordClass r "" _):x) =  map toUpper r
showRes ws@((WordClass r q _):wqs) = "@" ++ (s r ws) where
  s r [] = "(" ++ map toUpper r ++ ")"
  s r (wc:wcs) = map toUpper (qual wc) ++ s r wcs


data SqBase = SqBase FilePath

instance Argument SqBase where
  argumentType Proxy = "path-to-the-sqlite-database"
  parseArgument f = Just (SqBase f)

instance HasArguments SqBase where
  argumentsParser = atomicArgumentsParser

data Options = Options {
  dummy :: Bool          
} deriving (Show, Generic, HasArguments)

mods = [
       ]

