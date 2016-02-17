module Data.Trie where

import qualified Data.List as L

data Trie k v = Node { keys :: [k]
                     , value :: Maybe v
                     , children :: [Trie k v]
                     } deriving (Show, Eq)

fromList :: (Eq k) => [([k], v)] -> Trie k v
fromList xs = L.foldl' f (Node [] Nothing []) xs
  where f trie (ks, v) = insert ks v trie

insert :: (Eq k) => [k] -> v -> Trie k v -> Trie k v
insert ks v (Node nodeKeys oldVal children) =
  case decomposePrefix ks nodeKeys of
    (common, [], []) -> Node common (Just v) children

    (common, extras, []) ->
      case findSibling extras children of
        Nothing -> let newChild = Node extras (Just v) []
                   in Node common oldVal (newChild : children)
        Just (sib, rest) -> let newChild = insert extras v sib
                            in Node common oldVal (newChild : rest)

    (common, [], extras) -> Node common (Just v) [(Node extras oldVal children)]

    (common, newExtras, oldExtras) ->
      Node common Nothing [ Node oldExtras oldVal children
                          , Node newExtras (Just v) []
                          ]

findSibling :: (Eq k) => [k] -> [Trie k v] -> Maybe (Trie k v, [Trie k v])
findSibling [] _ = Nothing
findSibling (k:ks) tries = case span (not . startsWith k) tries of
                             (xs, y:ys) -> Just (y, xs ++ ys)
                             _ -> Nothing
  where startsWith k trie = case keys trie of
                              x:xs | x == k -> True
                              _ -> False

decomposePrefix :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
decomposePrefix (x:xs) (y:ys) | x == y = let (prefix, xs', ys') = decomposePrefix xs ys
                                         in (x:prefix, xs', ys')
decomposePrefix xs ys = ([], xs, ys)
