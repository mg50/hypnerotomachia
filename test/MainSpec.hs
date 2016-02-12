module MainSpec where

import Test.Hspec
import Syllable

spec :: Spec
spec = do
    describe "main" $ do
      it "works" $ do
        1 `shouldBe` 1
