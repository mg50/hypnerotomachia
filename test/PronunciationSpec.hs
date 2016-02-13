module PronunciationSpec where

import Test.Hspec
import Pronunciation

parsePhonemes = words

spec :: Spec
spec = do
  describe "stressIsValid" $ do
    it "recognizes valid examples" $ do
      stressIsValid [] `shouldBe` True
      stressIsValid [Long] `shouldBe` True
      stressIsValid [Short] `shouldBe` True
      stressIsValid [Long, Short, Long] `shouldBe` True
      stressIsValid [Short, Long, Short, Long, Short] `shouldBe` True

    it "recognizes invalid examples" $ do
      stressIsValid [Long, Long] `shouldBe` False
      stressIsValid [Short, Short] `shouldBe` False
      stressIsValid [Short, Long, Short, Short] `shouldBe` False

  describe "stressPattern" $ do
    it "parses 'internist'" $ do
      stressPattern (parsePhonemes "IH2 N T ER1 N IH0 S T") `shouldBe` [Short, Long, Short]

    it "parses 'ionize'" $ do
      stressPattern (parsePhonemes "AY1 AH0 N AY2 Z") `shouldBe` [Long, Short, Long]

    it "parses 'jurisdiction'" $ do
      stressPattern (parsePhonemes "JH UH2 R AH0 S D IH1 K SH AH0 N")
        `shouldBe` [Long, Short, Long, Short]

    it "parses 'windowpanes'" $ do
      stressPattern (parsePhonemes "W IH1 N D OW0 P EY2 N Z")
        `shouldBe` [Long, Short, Long]
