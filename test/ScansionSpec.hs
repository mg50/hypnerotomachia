module ScansionSpec where

import System.Random
import Test.Hspec
import Pronunciation
import Scansion
import Control.Monad

spec :: Spec
spec = do
  describe "scansionTemplate" $ do
    it "creates templates with the correct length" $ do
      let results = map scansionTemplate $ map mkStdGen [100..200]
      forM_ results $ \(result, _) -> do
        length (concat result) `shouldBe` syllablesPerLine
