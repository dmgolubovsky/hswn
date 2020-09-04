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
import Data.List (nub, isSuffixOf, take, partition, intercalate)
import Control.Monad.HT (for)
import Control.Monad.Loops
import System.FilePath
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

-- Line pattern example:
-- 1: We always wanted "Nn" to be "aA"
-- And "Nn" newer happened to "Vv" "nN" (1)

newtype IntField = IntField {
  fromIntField :: Int
}

instance FromRow IntField where fromRow = IntField <$> field

-- Parsed lyrics line

data ParsedLine = ParsedLine {
  lineLabel :: Maybe String
 ,lineWords :: [LineWord] -- words are in reverse so the rhyme can be easily found
} deriving (Show)

data LineWord = FixedWord String
              | WordPattern String
              | RhymedPattern String String
     deriving (Show)

-- Parser stuff

pLine :: Parser ParsedLine

pLine = do
  lw <- (pLabel >>= return . Left) <|> (pWord >>= return . Right)
  many1 $ char ' '
  wds <- pWord `sepBy` (many1 $ char ' ')
  many1 endOfLine
  case lw of
    Left ll -> return $ ParsedLine (Just ll) (reverse wds)
    Right w -> return $ ParsedLine Nothing (reverse (w : wds))
  
pLabel :: Parser String

pLabel = manyTill digit (char ':')

pWord :: Parser LineWord

pWord = (rp >>= return . uncurry RhymedPattern) <|> (fx >>= return . FixedWord) <|> (wp >>= return . WordPattern) where
  fx = many1 letter
  wp = char '"' >> manyTill letter (char '"')
  rp = do
    char '"'
    w1 <- fx
    w2 <- char '/' *> manyTill digit (char '"')
    return (w1, w2)

parseLyrics :: FilePath -> IO [ParsedLine]

parseLyrics rhypat = do
  pat <- TIO.readFile rhypat
  TIO.putStrLn pat
  let res = prs [] (many1 pLine) pat where
        prs ll p pat | T.null pat = ll
        prs ll p pat = let res = parse p pat in case res of
                         Done i r -> prs (ll ++ r) p i
                         Partial x -> let res1 = feed res T.empty in case res1 of
                           Done i r -> prs (ll ++ r) p i
                           _ -> error $ "nested parse failure: " ++ show res1
                         Fail _ _ _ -> error $ "parse failure: " ++ show res
  return res

-- Generate lyrics from a parsed template.

genLyrics :: Connection -> [ParsedLine] -> IO [String]

genLyrics conn lns = gl conn lns [] M.empty M.empty where
  getWord _ _ (FixedWord w) = return w
  getWord _ _ (WordPattern p) = getRandomWordQual conn p
  getWord rhymap _ (RhymedPattern p r) = do
    let rhw = M.lookup r rhymap
    case rhw of
      Nothing -> getRandomWordQual conn p
      Just rh -> do
        rhs <- allRhymes conn rh 1 p p >>= return . (map word)
        case rhs of
          [] -> getRandomWordQual conn p
          _ -> return $ head rhs
  gl conn [] acc rhymap refmap = return acc
  gl conn (l:lns) acc rhymap refmap = do
    ln <- mapM (getWord rhymap refmap) (lineWords l)
    let rhymap' = case lineLabel l of
                    Nothing -> rhymap
                    Just lab -> M.insert lab (head ln) rhymap
    let refmap' = refmap
    let acc' = acc ++ reverse ln ++ ["\n"]
    gl conn lns acc' rhymap' refmap'


-- SQL Database Path -> Lyrics pattern path

main' :: SqBase -> LyrPat -> Options -> IO ()

main' (SqBase sqfp) (LyrPat rhypat) opts = do
  conn <- open sqfp
  case (corpus opts) of
    Just fp -> makeCorpus conn fp
    Nothing -> execute_ conn [sql|create temporary table corpus as select distinct * from
                                     (select * from adverb union 
                                      select * from adject union 
                                      select * from noun union 
                                      select * from verb)
                                        where numvow <= 4 and
                                              word not like '% %' and
                                              word not like '%-%'|] 
  (IntField nw):_ <- query_ conn [sql|select count(distinct word) from corpus|] :: IO [IntField]
  putStrLn $ "Corpus: " ++ show nw ++ " words"
  lns <- parseLyrics rhypat
  lrs <- genLyrics conn lns
  mapM (\s -> putStr (" " ++ s)) lrs
  close conn
  return ()

-- Find a proper word for an altered one

properWord :: Connection -> String -> IO [String]

properWord conn w = do
  res <- queryNamed conn [sql|select distinct altered, proper from
                                (select * from advexc union 
                                 select * from adjexc union 
                                 select * from nounexc union 
                                 select * from verbexc)
                                   where altered = :word|] [":word" := w] :: IO [(String, String)]
  case res of
    [] -> return [w]
    wc:_ -> return [fst wc, snd wc]


suffixes = ["", "s", "'s", "es", "d", "ed", "ing"]

stripSuffix :: String -> [String]

stripSuffix w = concatMap (onesfx w) suffixes where
  onesfx w "" = [w]
  onesfx w s | s `isSuffixOf` w = [w, take ln w] where ln = length w - length s
  onesfx _ _ = []

-- Given a text file make the corpus table out of it

makeCorpus :: Connection -> FilePath -> IO ()

makeCorpus conn fp = do
  w0 <- readFile fp
  let w = filter ((> 3) . length) . nub $ map (filter isAlpha) $ words $ map toLower w0
  let w1 = concatMap stripSuffix w
  w2 <- mapM (properWord conn) w >>= return . concat
  let wq = "'" ++ intercalate "','" (nub $ w1 ++ w2) ++ "'"
  execute_ conn (Query $ T.pack (
    "create temporary table corpus as select distinct * from " ++
    "  (select * from adverb union " ++
    "   select * from adject union " ++
    "   select * from noun union " ++
    "   select * from verb) where word in (" ++ wq ++ ")"))

-- Drop elements from the end of a list

dropEnd n = reverse . (drop n) . reverse

-- Take an element from the end of a list

takeEnd n = reverse . (take n) . reverse

-- Make one line given the last word, sentence structure, and rhytmic pattern.
-- The last word is expected to come from allRhymes and be in agreement with
-- the sentence structure and rhytmic pattern.

makeLine :: Connection -> String -> String -> IPAData -> IO [String]

makeLine conn rhypat stnc ipd = do
  let rhypat' = dropEnd (numvow ipd) rhypat
      stnc' = dropEnd 1 stnc
  mkl [word ipd] rhypat' stnc' where
    mkl ws [] [] = return ws
    mkl ws [] stn = return (take (length stn) (repeat "*") ++ ws)
    mkl ws rhy [] = return (take (length rhy) (repeat ".") ++ ws)
    mkl ws rhy stn = do
      let patstr = findPatStress rhy
          ql = map toLower $ takeEnd 1 stn
          maxvow = length rhy
          highvow = let z = (length rhy `div` length stn) in if z < patstr + 1 then patstr + 1 else z
          b1 = (if (length stn) == 1 then 0 else 1) :: Int
      nxipa1 <- queryNamed conn [sql|select distinct * from corpus 
                                        where qual = :ql and
                                              stress = :str and
                                              (numvow <= :highvow and 1 = :b1 or numvow = :maxvow and 0 = :b1) 
                                          order by random(), numvow limit 1|] 
                                            [":ql" := ql, ":str" := patstr, ":maxvow" := maxvow, ":b1" := b1, ":highvow" := highvow] :: IO [IPAData]
      nxipa2 <- queryNamed conn [sql|select distinct * from corpus
                                       where qual = :ql and
                                             numvow = 1
                                         order by random() limit 1|] [":ql" := ql] :: IO [IPAData]
      let nxipa = take 1 $ nxipa1 ++ nxipa2
      case nxipa of
        [] -> if ql `elem` (map show [1 .. 9])
                then do
                        let mwords = ["FOO", "BAR", "BAZ", "QUX", "QUUX", "QUUZ", "CORG", "WALD", "FRED", "PLUGH"]
                            mwidx = (read ql) :: Int
                            nxws = (mwords !! (mwidx - 1)) : ws
                            nxrhy = dropEnd 1 rhy
                            nxstn = dropEnd 1 stn
                        mkl nxws nxrhy nxstn
                else do
                        putStrLn $ "cannot find a word ql=" ++ ql ++ " stress=" ++ show patstr ++ " numvow<=" ++ show highvow
                        return ws
        ip:ips -> do
          let nxws = word ip : ws
              nxrhy = dropEnd (numvow ip) rhy
              nxstn = dropEnd 1 stn
          mkl nxws nxrhy nxstn
        
 

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

allRhymes :: Connection -> String -> Int -> String -> String -> IO [IPAData]

allRhymes conn rword nrhy rhypat stnc = do
  let rq = toLower $ head (reverse stnc)
  res <- queryNamed conn [sql|select distinct * from corpus
                                   where word = :word|] [":word" := map toLower rword] :: IO [IPAData]
  case res of
    [] -> error $ "cannot retrieve information for " ++ rword
    (rw:_) -> do
      let patstr = findPatStress rhypat
      if stress rw /= patstr
        then putStrLn $ "warning: pattern stress =/= word stress"
        else return ()
      let h1 = nrhy `div` 2
          h2 = nrhy - h1
      let getwords cmp dir nw = query_ conn (Query $ T.pack $ 
           "select distinct * from corpus " ++
                " where rhyfd " ++ cmp ++ "'" ++ rhyfd rw ++ "' " ++
                " and stress = " ++ show patstr ++ 
                " and numvow <= " ++ show (numvow rw) ++
                " and qual = '" ++ [rq] ++ "' " ++
                  " order by rhyfd " ++ dir ++ " limit " ++ show (if nw < 0 then 0 else nw)) :: IO [IPAData]
      rhys1 <- getwords ">" "asc" h1
      rhys2 <- getwords "<" "desc" h2
      return (rhys1 ++ rhys2)


-- Get a random word from the entire database wrt the desired rhyming pattern

getRandomWord :: Connection -> String -> IO String

getRandomWord conn rhypat = do
  let patstr = findPatStress rhypat
  res <- queryNamed conn [sql|select distinct word, qual from corpus
                                where word not like '% %' and stress = :stress
                              order by random() limit 1|] [":stress" := patstr] :: IO [(String, String)]
  case res of
    [] -> error "cannot make random selection"
    x  -> return $ fst (head res)

-- Get a random word from the entire database wrt the desired rhyming pattern and the word qualifier (the first letter of the pattern)

getRandomWordQual :: Connection -> String -> IO String

getRandomWordQual conn rhypat = do
  let patstr = findPatStress rhypat
      qual = map toLower (take 1 rhypat)
      nvow = length rhypat
  res <- queryNamed conn [sql|select distinct word, qual from corpus
                                where word not like '% %' and stress = :stress and qual = :qual and numvow = :nvow
                              order by random() limit 1|] [":stress" := patstr, ":qual" := qual, ":nvow" := nvow] :: IO [(String, String)]
  case res of
    [] -> error "cannot make random selection"
    x  -> return $ fst (head res)


data SqBase = SqBase FilePath

instance Argument SqBase where
  argumentType Proxy = "path-to-the-sqlite-database"
  parseArgument f = Just (SqBase f)

instance HasArguments SqBase where
  argumentsParser = atomicArgumentsParser


data LyrPat = LyrPat FilePath

instance Argument LyrPat where
  argumentType Proxy = "lyrics-pattern"
  parseArgument f = Just (LyrPat f)

instance HasArguments LyrPat where
  argumentsParser = atomicArgumentsParser


data Options = Options {
  corpus :: Maybe String
} deriving (Show, Generic, HasArguments)


mods :: [Modifier]

mods = [
  AddShortOption "corpus" 'c'
 ,AddOptionHelp  "corpus" "Path to the corpus file (or /dev/stdin)"
       ]



