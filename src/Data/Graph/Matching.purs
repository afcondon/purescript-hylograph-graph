-- | Bipartite matching.
-- |
-- | `maximumBipartiteMatching` finds a maximum matching by Kuhn's
-- | augmenting-path algorithm, O(V·E) — the right tool for small dense
-- | graphs (constraint-propagation houses, assignment problems); swap in
-- | Hopcroft–Karp only if profiling ever demands it. The input is the
-- | left-side adjacency (each `a` with the set of `b`s it may pair with);
-- | the result maps each matched `b` to its `a`.
-- |
-- | Contributed for Régin-style alldifferent filtering (matching + the
-- | strongly connected components of the alternating digraph decide which
-- | candidate edges survive in some maximum matching), but nothing here is
-- | specific to that use.
module Data.Graph.Matching
  ( maximumBipartiteMatching
  , matchingSize
  , isPerfect
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set

-- | Maximum bipartite matching, right-to-left: each matched `b` with the
-- | `a` it is assigned to.
maximumBipartiteMatching :: forall a b. Ord a => Ord b => Map a (Set b) -> Map b a
maximumBipartiteMatching adjacency =
  foldl tryAugment Map.empty (Set.toUnfoldable (Map.keys adjacency) :: Array a)
  where
  tryAugment matching a = case augment a Set.empty matching of
    Right matched -> matched
    Left _ -> matching

  -- Left: failed, with the visited set enlarged — threaded through the
  -- whole attempt, which is the essence of Kuhn's. Right: augmented.
  augment a visited matching =
    foldl step (Left visited) (neighbours a)
    where
    step (Right m) _ = Right m
    step (Left vis) b
      | Set.member b vis = Left vis
      | otherwise =
          let
            vis' = Set.insert b vis
          in
            case Map.lookup b matching of
              Nothing -> Right (Map.insert b a matching)
              Just a' -> case augment a' vis' matching of
                Right m' -> Right (Map.insert b a m')
                Left vis'' -> Left vis''

  neighbours a = fromMaybe [] (Set.toUnfoldable <$> Map.lookup a adjacency)

matchingSize :: forall a b. Map b a -> Int
matchingSize = Map.size

-- | Every left vertex matched.
isPerfect :: forall a b. Ord a => Ord b => Map a (Set b) -> Map b a -> Boolean
isPerfect adjacency matching = Map.size matching == Map.size adjacency
