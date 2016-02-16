module Sonnet (generateSonnet) where

import qualified Data.Map as M
import System.Random
import Control.Monad
import qualified Data.Sequence as S
import Data.List

import Pronunciation
import Dictionary
import Scansion
import Rhyme

generateSonnet dict trie = do
  stanzas <- replicateM 3 (generateStanza dict trie)
  (end1, end2) <- generateRhymingLines dict trie
  return $ concat stanzas ++ [end1, end2]

generateStanza dict trie = do
  (line1, line3) <- generateRhymingLines dict trie
  (line2, line4) <- generateRhymingLines dict trie
  return [line1, line2, line3, line4]

generateRhymingLines dict trie = do
  gen <- fmap mkStdGen randomIO
  let ((w1, s1), (w2, s2)) = generateRhymingPairs gen trie
      mkTemplate stresses gen = scansionTemplate (syllablesPerLine - length stresses) gen
      (t1, gen') = mkTemplate s1 gen
      (t2, gen'') = mkTemplate s2 gen'
  line1 <- forM t1 (getRandomWordWithStresses dict)
  line2 <- forM t2 (getRandomWordWithStresses dict)
  return (format $ line1 ++ [w1], format $ line2 ++ [w2])

generateRhymingPairs gen trie =
  let (result, travState) = runTraversal gen trie getRhymes
      gen' = randomSeed travState
  in case result of
     Nothing -> generateRhymingPairs gen' trie
     Just result@((word1,pron1), (word2,pron2)) ->
       let (s1, s2) = (stressPattern pron1, stressPattern pron2)
       in if last s1 == Long && last s2 == Long
          then ((word1,s1), (word2, s2))
          else generateRhymingPairs gen' trie

-- go = do
--   commonWords <- fmap (Set.fromList . lines) $ readFile "./resources/common_words.txt"
--   pronunciationLines <- fmap lines $ readFile "./resources/cmudict.txt"
--   let dictEntries = map parseLine pronunciationLines
--       wordIsCommon (word,_) = Set.member word commonWords
--       commonEntries = (filter wordIsCommon dictEntries)
--       trie = createRhymeTrie commonEntries
--   print "building trie"
--   trie `seq` print "done"

--   forM_ [1..10] $ \_ -> do
--     gen <- fmap mkStdGen randomIO
--     let result = generateRhymingPairs gen trie
--     print result


format :: [String] -> String
format words = intercalate " " words

getRandomWordWithStresses :: Dictionary -> [Stress] -> IO String
getRandomWordWithStresses dict stresses =
  case M.lookup stresses dict of
    Nothing -> error $ "could not find word with pattern " ++ show stresses
    Just words -> do let len = S.length words
                     idx <- randomRIO (0, len - 1)
                     return (S.index words idx)
