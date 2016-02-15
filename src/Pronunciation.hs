{-# LANGUAGE FlexibleContexts #-}
module Pronunciation where
import Text.Regex.PCRE

type Phoneme = String
type Pronunciation = [Phoneme]

data Stress = Long | Short deriving (Eq, Show, Ord)

stressIsValid :: [Stress] -> Bool
stressIsValid [] = True
stressIsValid (Long:Long:rest) = False
stressIsValid (Short:Short:rest) = False
stressIsValid (_:rest) = stressIsValid rest

stressPattern :: Pronunciation -> [Stress]
stressPattern = go . map stressNumber . vowels
  where stressNumber phoneme = phoneme =~ "\\d+$" :: String
        go [] = []
        go ("0":rest) = Short : go rest
        go ("1":[]) = [Long]
        go ("1":"1":rest) = Long : go ("1":rest)
        go ("1":_:rest) = [Long, Short] ++ go rest
        go ("2":[]) = [Long]
        go ("2":"1":rest) = Short : go ("1":rest)
        go ("2":next:rest) = Long : go (next:rest)

vowels :: Pronunciation -> [Phoneme]
vowels = filter hasStress
  where hasStress phoneme = phoneme =~ "\\d$"
