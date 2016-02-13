module Dictionary where

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List
import Pronunciation

type Dictionary = M.Map [Stress] (S.Seq String)

createDictionary :: [(String, Pronunciation)] -> Dictionary
createDictionary entries =
  let entryValid (_,_,stresses) = stressIsValid stresses
      validEntries = filter entryValid $ map addStressPattern entries

      go :: Dictionary -> (String, Pronunciation, [Stress]) -> Dictionary
      go dict (word, _, stresses) =
        case M.lookup stresses dict of
          Nothing -> M.insert stresses (S.singleton word) dict
          Just vals -> M.insert stresses (vals S.|> word) dict

  in foldl' go M.empty validEntries


addStressPattern (s, pron) = (s, pron, stressPattern pron)
