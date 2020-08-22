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
  word :: String         -- word itself
 ,root :: String         -- part of word that matched
 ,qual :: String         -- part of speech detected (empty string if not detected)
 ,suffix :: String       -- suffix that was removed to match
} deriving (Eq, Show)


unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs

classWord :: Connection -> String -> IO [WordClass]

classWord conn w = do
  res <- queryNamed conn [sql|select distinct word, qual from 
                                (select * from adverb union 
                                 select * from adject union 
                                 select * from noun union 
                                 select * from verb) 
                                   where word = :word|] [":word" := w] :: IO [(String, String)]
  case res of
    [] -> return [WordClass w w "" ""]
    rs@((r, q):_) -> return $ unzipWith wc rs where wc a b = WordClass w a b ""


properWord :: Connection -> String -> IO WordClass

properWord conn w = do
  res <- queryNamed conn [sql|select distinct altered, proper from
                                (select * from advexc union 
                                 select * from adjexc union 
                                 select * from nounexc union 
                                 select * from verbexc)
                                   where altered = :word|] [":word" := w] :: IO [(String, String)]
  case res of
    [] -> return $ WordClass w w "" ""
    wc:_ -> return $ WordClass (fst wc) (snd wc) "" "-"


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
    prop@(WordClass ww rr qq ss) <- properWord conn w
    let ssfx = stripSuffix rr
    res' <- for ssfx $ \(wd, sx) -> do
      let hasQual (WordClass _ _ "" "") = True
          hasQual (WordClass _ _ q _) | length q > 0 = True
          hasQual (WordClass _ _ "" s) | length s > 0 = False
          hasQual (WordClass _ _ _ _) = False
      rr <- classWord conn wd >>= return . nub
      rr' <- for rr (\(WordClass _ r q _) -> return $ WordClass w r q sx)
      return $ filter hasQual rr'
    let res = concat res'
    let partGrp s = case partition (\(WordClass _ _ q _) -> length q == 0) s of
          ([], []) -> []
          (s1, []) -> take 1 s1
          (_, s2) -> s2
    let pres = partGrp res
    putStrLn $ showRes pres
  close conn
  return ()

showRes :: [WordClass] -> String

showRes [] = "<Nothing>"
showRes ((WordClass w r "" _):x) =  map toUpper w
showRes ws@((WordClass w r q _):wqs) = "@" ++ (s r ws) where
  s r [] = "(" ++ map toUpper w ++ "/" ++ map toUpper r ++ ")"
  s r (wc:wcs) = case (qual wc `elem` map qual wcs) of
    True -> s r wcs
    False -> map toUpper (qual wc) ++ s r wcs


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

