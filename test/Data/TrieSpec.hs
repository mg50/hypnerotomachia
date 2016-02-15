module Data.TrieSpec where

import Test.Hspec
import Data.Trie

spec :: Spec
spec = do
  describe "fromList" $ do
    it "creates a trie from a large list" $ do
      let entries = [ (["A"], 2)
                    , (["A", "B", "C", "D"], 3)
                    , (["A", "B", "C", "E"], 10)
                    , (["A", "X", "Y"], 4)
                    , (["Q"], 5)
                    ]
          result = fromList entries

      result `shouldBe`
        Node [] Nothing
        [ Node ["Q"] (Just 5) []
        , Node ["A"] (Just 2)
          [ Node ["X", "Y"] (Just 4) []
          , Node ["B", "C"] Nothing
            [ Node ["D"] (Just 3) []
            , Node ["E"] (Just 10) [] ]
          ]
        ]

    it "asdf" $ do
      let entries = [ (["ER", "V", "AH", "HH"], "hover")
                    , (["ER", "V", "AH", "K"], "cover")
                    , (["OW", "L", "EH", "HH"], "hello")
                    ]
          result = fromList entries

      result `shouldBe`
        Node [] Nothing
        [ Node ["OW", "L", "EH", "HH"] (Just "hello") []
        , Node ["ER", "V", "AH"] Nothing
          [ Node ["HH"] (Just "hover") []
          , Node ["K"] (Just "cover") []
          ]

        ]

  describe "insert" $ do
    it "inserts into an empty trie" $ do
      let trie = Node [] Nothing []
          result = insert ["A"] 2 trie
      result `shouldBe` Node [] Nothing [Node ["A"] (Just 2) []]

    it "inserts a more complex key into an empty trie" $ do
      let trie = Node [] Nothing []
          result = insert ["A", "B"] 2 trie
      result `shouldBe` Node [] Nothing [Node ["A", "B"] (Just 2) []]

    it "inserts a prefix of an existing key sequence" $ do
      let trie = Node ["A", "B", "C"] (Just 2) []
          result = insert ["A", "B"] 3 trie
      result `shouldBe` Node ["A", "B"] (Just 3) [Node ["C"] (Just 2) []]

    it "splits nodes when necessary" $ do
      let trie = Node ["A"] (Just 2) [ Node ["B", "C"] (Just 3) []
                                     , Node ["X", "Y"] (Just 4) []
                                     ]
          result = insert ["A", "B"] 10 trie

      result `shouldBe`
        Node ["A"] (Just 2)
        [ Node ["B"] (Just 10)
          [ Node ["C"] (Just 3) [] ]
        , Node ["X", "Y"] (Just 4) []
        ]

    it "works in a complex case" $ do
      let trie = Node ["A"] (Just 2) [ Node ["B", "C"] (Just 3) []
                                     , Node ["X", "Y"] (Just 4) []
                                     ]
          result = insert ["A", "B", "D"] 10 trie

      result `shouldBe`
        Node ["A"] (Just 2)
        [ Node ["B"] Nothing
          [ Node ["C"] (Just 3) []
          , Node ["D"] (Just 10) [] ]
        , Node ["X", "Y"] (Just 4) []
        ]

    it "works in an even more complex case" $ do
      let trie = Node ["A"] (Just 2) [ Node ["B", "C", "D"] (Just 3) []
                                     , Node ["X", "Y"] (Just 4) []
                                     ]
          result = insert ["A", "B", "C", "E"] 10 trie

      result `shouldBe`
        Node ["A"] (Just 2)
        [ Node ["B", "C"] Nothing
          [ Node ["D"] (Just 3) []
          , Node ["E"] (Just 10) [] ]
        , Node ["X", "Y"] (Just 4) []
        ]
