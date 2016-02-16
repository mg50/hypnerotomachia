{-# LANGUAGE FlexibleContexts #-}
module Rhyme (createRhymeTrie, getRhymes, runTraversal, TraversalState(..)) where
import Pronunciation

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Trie as T

import Text.Regex.PCRE

import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.Random

type RhymeTrie = T.Trie RhymeSound (String, Pronunciation)
data RhymeSound = Vowel String Stress | Consonant String deriving (Show, Eq, Ord)

data TraversalState = TraversalState { currentNode :: RhymeTrie
                                     , randomSeed :: StdGen
                                     } deriving (Show)

type TraversalM = State TraversalState

--------------------------------------------------------------------------------
-- Creating a rhyme trie
--------------------------------------------------------------------------------

createRhymeTrie :: [(String, Pronunciation)] -> RhymeTrie
createRhymeTrie pairs =
  let mapper (word, pron) = (reverse (rhymeSounds pron), (word, pron))
      pairs' = map mapper pairs
  in T.fromList pairs'

rhymeSounds :: Pronunciation -> [RhymeSound]
rhymeSounds pron = go pron (stressPattern pron)
  where go [] _ = []
        go (phoneme:rest) stresses =
          case getVowel phoneme of
            Nothing -> Consonant phoneme : go rest stresses
            Just vowel -> Vowel vowel (head stresses) : go rest (tail stresses)

        getVowel phoneme = case phoneme =~ "([A-Z]+)(\\d+)" :: [[String]] of
                             [] -> Nothing
                             match -> Just (match !! 0 !! 1)

--------------------------------------------------------------------------------
-- Random rhyme trie traversal
--------------------------------------------------------------------------------

runTraversal :: StdGen -> RhymeTrie -> TraversalM a -> (a, TraversalState)
runTraversal gen trie m = runState m (TraversalState trie gen)

getRhymes :: TraversalM (Maybe ((String, Pronunciation), (String, Pronunciation)))
getRhymes = do
  result <- goToRandomTerminalVowelNode
  current <- gets currentNode
  rhyme1 <- case result of
              Just word -> return word
              Nothing -> getRhymeWordFrom current
  rhyme2 <- getRhymeWordFrom current
  if rhyme1 == rhyme2
     then return Nothing
     else return $ Just (rhyme1, rhyme2)

  where getRhymeWordFrom node =
          do goTo node
             goToRandomLeaf
             current <- gets currentNode
             case T.value current of
               Just val -> return val
               Nothing -> error "should not reach here!"

goToRandomLeaf :: TraversalM ()
goToRandomLeaf = do
  current <- gets currentNode
  l <- isLeaf
  let children = T.children current
      len = length children
      num = if l then len else len - 1
  idx <- getRandom (0, num)
  if idx == len
     then return ()
     else do goTo (children !! idx)
             goToRandomLeaf

-- A terminal vowel node either starts a word or has no vowel descendants
goToRandomTerminalVowelNode :: TraversalM (Maybe (String, Pronunciation))
goToRandomTerminalVowelNode = do
  goToRandomVowel
  current <- gets currentNode
  loop current

  where loop previousVowelNode = do
          current <- gets currentNode
          vowel <- isVowel
          let children = T.children current
              value = T.value current
          case (vowel, value, children) of
            -- Node is a vowel with no children
            (True, _, []) -> return Nothing

            -- Node is a consonant with no children
            (False, _, []) -> do goTo previousVowelNode
                                 return Nothing

            -- Node is a word starting with a vowel
            (True, Just v, cs) -> do r <- getRandom (0, 3)
                                     if r == 0
                                       then return (Just v)
                                       else do goToRandomChild
                                               loop current

            -- Node is vowel but not a word; keep looking for more
            (True, Nothing, cs) -> do goToRandomChild
                                      loop current

            -- Node is not a vowel, but has descendents; keep searching
            (False, _, cs) -> do goToRandomChild
                                 loop previousVowelNode

goToRandomVowel :: TraversalM ()
goToRandomVowel = do
  current <- gets currentNode
  v <- isVowel
  if v
     then return ()
     else do goToRandomChild
             goToRandomVowel

goToRandomChild :: TraversalM ()
goToRandomChild = do current <- gets currentNode
                     let children = T.children current
                     case children of
                       [] -> error "cannot go to empty children"
                       _ -> do rand <- getRandom (0, length children - 1)
                               goTo (children !! rand)

isVowel :: TraversalM Bool
isVowel = do current <- gets currentNode
             let soundIsVowel (Vowel _ _) = True
                 soundIsVowel _ = False
             case L.find soundIsVowel (T.keys current) of
               Just _ -> return True
               _ -> return False

goTo :: RhymeTrie -> TraversalM ()
goTo node = modify $ \state -> state{currentNode = node}

hasChildren :: TraversalM Bool
hasChildren = do current <- gets currentNode
                 case (T.children current) of
                   [] -> return False
                   _ -> return True

isLeaf :: TraversalM Bool
isLeaf = do current <- gets currentNode
            return $ case (T.value current) of
                       Just _ -> True
                       Nothing -> False

getRandom :: (Int, Int) -> TraversalM Int
getRandom (lo,hi) = do gen <- gets randomSeed
                       let (num,gen') = randomR (lo,hi) gen
                       modify $ \state -> state{randomSeed = gen'}
                       return num
