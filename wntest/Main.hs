-- Basic testing of WordNet
--

module Main where

import NLP.WordNet


main = do
  wn <- initializeWordNetWithOptions (Just "/usr/share/wordnet") Nothing
  ov <- runWordNetWithOptions (Just "/usr/share/wordnet") Nothing $ getOverview "program"
  putStrLn $ show ov
  closeWordNet wn
  return ()
