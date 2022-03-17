-- Basic testing of WordNet
--

module Main where

import NLP.WordNet


main = do
  wn <- initializeWordNetWithOptions (Just "/usr/share/wordnet") Nothing
  let ov = runs wn $ getOverview "program"
  putStrLn $ show ov
  let srs = runs wn $ search "program" Noun AllSenses
  let rsrc = runs wn $ relatedBy Similar (head srs)
  mapM (putStrLn . show) (map srOverview srs)
  closeWordNet wn
  return ()
