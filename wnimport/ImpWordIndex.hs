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

import IpaMap

data IPAAttr = IPAUnCat | IPAAttr {
  isVowel :: Bool                    -- is this a vowel?
 ,refined :: Char                    -- equivalent character used for approx. rhyming
} deriving (Eq, Show)

type IPAMap = M.Map IPA IPAAttr

-- Build a refined IPA map from given corpus of words and possibly nonempty existing IPA map

mkIPAMap :: [IPAWord] -> IPAMap -> IPAMap

mkIPAMap ws prev = m where
  allipa = nub $ concatMap ipa' ws
  m = t allipa M.empty
  t [] mp = mp
  t (i:is) mp = M.fromList (z i) `M.union` t is mp where
  z (IPA []) = []
  z i = case M.lookup i prev of
    Nothing -> [(i, IPAUnCat)]
    Just attr -> [(i, attr)]

-- Print an IPA map to the given handle
-- a:V:a
-- b:C:b

prtIPAMap :: Handle -> IPAMap -> IO ()

prtIPAMap h mp = w >> hFlush h where
  w = do
    let lst = M.toList mp
    hPutStrLn h "#:Please populate the uncategorized entries"
    mapM_ z lst where
      z (i, a) = l i >> r a
      l i = hPutStr h (fromIPA i) >> hPutStr h ":"
      r a = case a of
        IPAUnCat -> hPutStrLn h ""
        IPAAttr _ _ -> do
          case isVowel a of
            True -> hPutStr h "V"
            False -> hPutStr h "C"
          hPutStrLn h $ ":" ++ [refined a]

-- Split a line into IPA and attributes

l2a :: String -> Maybe (IPA, IPAAttr)

l2a s = x where
  toks = splitOneOf ":" s
  x = case toks of
    ("#":_ ) -> Nothing
    (i:vc:r:_) | i /= [] && r /= [] -> Just (toIPA i, IPAAttr iv rf) where
      iv = vc `elem` ["v", "V"]
      rf = head r
    (i:_) | i /= [] -> Just (toIPA i, IPAUnCat)
    _ -> Nothing

-- Read an IPA map from the embedded file

readIPAMap :: IPAMap

readIPAMap = M.fromList $ catMaybes attrs where
  ls = lines ipaMap
  attrs = map l2a ls


-- Represent an IPA token

data IPA = IPA [Word32] deriving (Eq, Ord, Show)
 
toIPA :: String -> IPA

toIPA s = IPA $ map (fromIntegral . ord) s

fromIPA :: IPA -> String

fromIPA (IPA i) = map (chr . fromIntegral) i

-- Run espeak to extract IPA from the utterance

getIPA :: String -> IO [IPA]

getIPA utter = do
  let esp = proc "espeak" [
        "-q",
        "--ipa",
        "--sep=_",
        "-v", "en-us",
        utter
        ] 
  (_, mbhout, _, p) <- createProcess $ esp { std_out = CreatePipe }
  case mbhout of
    Nothing -> error "Cannot get espeak handle"
    Just hout -> do
      s <- SIO.hGetContents hout >>= return . filter (`notElem` [' ', '\n'])
      waitForProcess p
      let splits = splitOneOf "_" s
      return $ map toIPA splits

data IPAWord = IPAWord {
  word :: String                     -- word proper
 ,ipa :: [IPA]                       -- original transcription
 ,ipa' :: [IPA]                      -- stress and long marks removed
 ,rfd :: String                      -- refined IPA (expressed in chars)
 ,rfdpat :: String                   -- vowel-consonant pattern for refined IPA
 ,rhyfd :: String                    -- refined IPA for rhyming (consonants condensed)
 ,numvow :: Int                      -- number of vowels
 ,stress :: Int                      -- stressed vowel position from end
} deriving (Ord, Eq, Show)

-- Initially construct an IPA word with minimal refinement that does not require
-- an attribute map.

ipaword :: String -> [IPA] -> IPAWord

ipaword w i = let i' = filter (not . hollow) i in IPAWord {
  word = w
 ,ipa = i'
 ,ipa' = map clean i'
 ,rfd = ""
 ,rfdpat = ""
 ,rhyfd = ""
 ,numvow = -1
 ,stress = -1
} where clean (IPA is) = IPA $ filter (`notElem` [716, 712, 720]) is
        hollow (IPA []) = True
        hollow _ = False

-- Refine an IPA word using the attribute map. If an uncategorized IPA symbol
-- occurs it is considered a consonant and goes into the refined IPA by taking
-- the first symbol of itself.

iparefine :: Bool -> IPAMap -> IPAWord -> IPAWord

iparefine sec mp ipaw = ipaw {
  rfd = rf
 ,rhyfd = map snd rhy
 ,rfdpat = pat
 ,numvow = length $ filter (== 'V') pat
 ,stress = length $ takeWhile (not . stressed . fst) $ vowrev
} where
  stressed (IPA is) = (712 `elem` is) || (sec && (716 `elem` is))
  vowrev = filter ((== 'V') . snd) $ zip (reverse $ ipa ipaw) (reverse pat)
  rf = map (refined . attr) (ipa' ipaw)
  pat = map (bool 'C' 'V' . isVowel . attr) (ipa' ipaw)
  refpat = zip pat rf
  conc [] = []
  conc (('V', c):vcs) = ('V', c):conc vcs
  conc (('C', c):vcs) = ('C', c):(conc $ dropWhile ((== 'C') . fst) vcs)
  rhyz = reverse $ conc refpat
  rhy = case rhyz of
    (('V', c):_) -> ('C', '_'):rhyz
    _ -> rhyz
  attr (IPA []) = error $ "kuku "
  attr i@(IPA is) = case M.lookup i mp of
    n | n == Nothing || n == Just IPAUnCat -> IPAAttr {
                                                isVowel = False
                                               ,refined = chr $ fromIntegral $ head is
                                              }
    Just ia -> ia


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
      putStrLn $ word ++ " " ++ q ++ " [" ++ concat chrs ++ "] [" ++ rfd iw' ++ "] [" ++ rhyfd iw' ++ "]"
      return ()
  return ()
