{-# LANGUAGE FlexibleContexts #-}
module Main where
import Text.Regex.PCRE
import qualified Data.Map as M
import Data.Char
import System.Random
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.List

import Pronunciation
import Dictionary
import Scansion

main :: IO ()
main = do
  commonWords <- fmap (Set.fromList . lines) $ readFile "./resources/common_words.txt"
  pronunciationLines <- fmap lines $ readFile "./resources/cmudict.txt"

  putStrLn "Loading dictionary into memory..."
  let dictEntries = map parseLine pronunciationLines
      wordIsCommon (word,_) = Set.member word commonWords
      dict = createDictionary (filter wordIsCommon dictEntries)
  dict `seq` putStrLn "Load complete."

  forM_ [1..14] $ \_ -> do
    gen <- getStdGen
    let (template, _) = scansionTemplate gen

    words <- forM template (getRandomWordWithStresses dict)
    putStrLn (format words)

format :: [String] -> String
format words = intercalate " " words

getRandomWordWithStresses :: Dictionary -> [Stress] -> IO String
getRandomWordWithStresses dict stresses =
  case M.lookup stresses dict of
    Nothing -> error $ "could not find word with pattern " ++ show stresses
    Just words -> do let len = S.length words
                     idx <- randomRIO (0, len - 1)
                     return (S.index words idx)

parseLine :: String -> (String, Pronunciation)
parseLine line =
  let regex = "^([^ ]*) *(.*)$"
      matchResults:_ = line =~ regex :: [[String]]
  in (map toLower (matchResults !! 1), words (matchResults !! 2))
