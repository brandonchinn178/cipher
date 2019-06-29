-- module Cipher.Search (findCompatible) where
module Cipher.Search where

import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Equivalence.Monad (MonadEquiv(..), runEquivM)
import Data.List (sortBy, tails)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Debug.Trace

-- | Given a list of words and a list of nodes associated with each word, find all
-- possible compatible paths (i.e. paths that don't result in Nothings along the
-- way) built by taking the cartesian product of the nodes from all the words and
-- combining them using the given discrimination function.
--
-- Algorithm:
--   1. Filter out all duplicate words
--   2. Sort words with the most shared letters coming earlier
--   3. Fold over sorted list with the discrimination function
findCompatible :: (Show n, Ord n) => (n -> n -> Maybe n) -> [(String, [n])] -> [n]
findCompatible _ [] = []
findCompatible _ [(_, ns)] = ns
findCompatible f wordsData = foldMaybes f . map snd . map foo . sortByCommonLetters . nubOrdOn fst $ wordsData
  where
    foo (word, ciphermaps) = traceShow (word, length ciphermaps) (word, ciphermaps)

sortByCommonLetters :: Ord ns => [(String, ns)] -> [(String, ns)]
sortByCommonLetters = collapse Set.empty . kruskal getNumCommonLetters . map addLetterBag
  where
    addLetterBag (word, ns) = (word, ns, Set.fromList word)
    getNumCommonLetters (_, _, letters1) (_, _, letters2) =
      -- negate because we want maximum spanning tree
      negate $ length $ Set.intersection letters1 letters2

    collapse _ [] = []
    collapse seen (curr:rest) =
      let ((word1, nodes1, _), (word2, nodes2, _)) = curr
          rest' = collapse (Set.insert word1 . Set.insert word2 $ seen) rest
          withWord2 = if Set.member word2 seen
            then rest'
            else (word2, nodes2) : rest'
          withWord1 = if Set.member word1 seen
            then withWord2
            else (word1, nodes1) : withWord2
      in withWord1

-- | Run Kruskal's algorithm, returning an minimum spanning tree of the input list.
--
-- https://stackoverflow.com/a/4293437/4966649
kruskal :: Ord a => (a -> a -> Int) -> [a] -> [(a, a)]
kruskal getWeight nodes = runEquivM (\_ -> ()) (\_ _ -> ()) $
  filterM go $ sortBy (comparing (uncurry getWeight)) edges
  where
    edges = [(node1, node2) | (node1:rest) <- tails nodes, node2 <- rest]
    go (grp1, grp2) = do
      isEq <- equivalent grp1 grp2
      if isEq
        then return False
        else equate grp1 grp2 >> return True

{- Helpers -}

-- | For a given input list `[as, bs, cs]`, combine every pairing between elements
-- in `as` and `bs` using the given discrimination function (used to combine and
-- determine if the computation should continue, at the same time).
foldMaybes :: (a -> a -> Maybe a) -> [[a]] -> [a]
foldMaybes _ [] = error "foldMaybes on empty list"
foldMaybes f as = flip foldr1 as $ \a a' -> catMaybes $ liftA2 f a a'
