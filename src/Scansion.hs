module Scansion (scansionTemplate, syllablesPerLine) where
import Control.Monad
import Control.Monad.State
import System.Random
import Data.List

import Pronunciation

type Weights = [(Double, [Stress])]

-- Iambic pentameter
syllablesPerLine :: Int
syllablesPerLine = 10

scansionTemplate :: Int -> StdGen -> ([[Stress]], StdGen)
scansionTemplate 0 gen = ([], gen)
scansionTemplate syllables gen = (stresses:restStresses, gen'')
  where weights = if even syllables then longWeights else shortWeights
        longestPossible = last . sort $ map (length . snd) weights
        limit = min syllables longestPossible
        validWeights = filter (\(_, pat) -> length pat <= limit) weights
        (stresses, gen') = selectRandomlyFrom validWeights gen
        (restStresses, gen'') = scansionTemplate (syllables - length stresses) gen'

selectRandomlyFrom :: Weights -> StdGen -> ([Stress], StdGen)
selectRandomlyFrom options gen =
  let intervals = tail $ scanl (+) 0.0 (map fst options)
      total = last intervals
      fractionalIntervals = map (/total) intervals
      zipper frac (_, val) = (frac, val)
      zipped = zipWith zipper fractionalIntervals options
      (rand, gen') = random gen
  in (snd . head $ dropWhile (\(num, _) -> num <= rand) zipped, gen')

shortWeights :: Weights
shortWeights = [ (4.0, [Short])
               , (3.0, [Short, Long])
               , (2.0, [Short, Long, Short])
               , (1.0, [Short, Long, Short, Long])
               , (1.0, [Short, Long, Short, Long, Short])
               ]

longWeights :: Weights
longWeights = [ (4.0, [Long])
              , (3.0, [Long, Short])
              , (2.0, [Long, Short, Long])
              , (1.0, [Long, Short, Long, Short])
              , (1.0, [Long, Short, Long, Short, Long])
              ]
