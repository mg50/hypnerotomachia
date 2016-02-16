{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Control.Monad
import Text.Regex.PCRE
import Data.Char
import Control.Monad
import Control.Monad.IO.Class
import System.Environment (getEnv)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text

import Pronunciation
import Dictionary
import Rhyme
import Sonnet


main :: IO ()
main = do
  commonWords <- fmap (Set.fromList . lines) $ readFile "./resources/common_words.txt"
  pronunciationLines <- fmap lines $ readFile "./resources/cmudict.txt"

  putStrLn "Loading dictionary into memory..."
  let dictEntries = map parseLine pronunciationLines
      wordIsCommon (word,_) = Set.member word commonWords
      commonEntries = (filter wordIsCommon dictEntries)
      dict = createDictionary commonEntries
      trie = createRhymeTrie commonEntries
  dict `seq` putStrLn "Load complete."

  print "Loading rhyme trie into memory..."
  trie `seq` print "Load complete."

  port <- read <$> getEnv "PORT"
  scotty port $ do
    middleware logStdoutDev
    get "/" $ do
      sonnetLines <- liftIO $ generateSonnet dict trie
      text $ Text.pack (unlines sonnetLines)

parseLine :: String -> (String, Pronunciation)
parseLine line =
  let regex = "^([^ ]*) *(.*)$" :: String
      matchResults:_ = line =~ regex :: [[String]]
  in (map toLower (matchResults !! 1), words (matchResults !! 2))
