module Data.Trie where
import qualified Data.List as L

data Trie k v = Node { keys :: [k]
                     , value :: Maybe v
                     , children :: [Trie k v]
                     } deriving (Show, Eq)

--fromList :: (Show k, Eq k) => [([k], v)] -> Trie k v
fromList xs = L.foldl' f (Node [] Nothing []) xs
  where f trie (ks, v) = insert ks v trie

--insert :: (Show k, Show v, Eq k) => [k] -> v -> Trie k v -> Trie k v
insert ks v (Node nodeKeys oldVal children) =
  case decomposePrefix ks nodeKeys of
    (common, [], []) -> Node common (Just v) []
    (common, extras, []) ->
      case findSiblingIndex extras children of
        Nothing -> let newChild = Node extras (Just v) []
                   in Node common oldVal (newChild : children)
        Just idx -> let oldChild = children !! idx
                        newChild = insert extras v oldChild
                        newChildren = listIndexSet idx newChild children
                    in Node common oldVal newChildren
    (common, [], extras) -> Node common (Just v) [(Node extras oldVal children)]
    (common, newExtras, oldExtras) -> Node common Nothing [ Node oldExtras oldVal children
                                                          , Node newExtras (Just v) []
                                                          ]

-- this needs to be optimized away
listIndexSet :: Int -> a -> [a] -> [a]
listIndexSet 0 x (y:ys) = x : ys
listIndexSet 0 _ [] = error "index does not exist"
listIndexSet n x (y:ys) = n `seq` y : listIndexSet (n - 1) x ys
listIndexSet _ _ _ = error "negative index does not work"

-- this needs to be optimized away
findSiblingIndex :: (Eq k) => [k] -> [Trie k v] -> Maybe Int
findSiblingIndex [] _ = Nothing
findSiblingIndex (k:ks) children =
  let isPrefixOf trie = L.isPrefixOf [k] (keys trie)
  in L.findIndex isPrefixOf children


-- somethingSibling :: (Eq k) => [k] -> [Trie k v] -> Maybe (Trie k v, [Trie k v])
-- somethingSibling [] _ = Nothing
-- somethingSibling (k:ks) tries = undefined -- go tries []
--   where go [] acc = acc
--         go (t:ts) acc = if L.isPrefixOf [k] (keys t)
--                         then  undefined
--                         else undefined

decomposePrefix :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
decomposePrefix (x:xs) (y:ys) | x == y = let (prefix, xs', ys') = decomposePrefix xs ys
                                         in (x:prefix, xs', ys')
decomposePrefix xs ys = ([], xs, ys)
