module Test.Matching (testMatching) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Graph.Matching (isPerfect, maximumBipartiteMatching)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)

adjacency :: Array (Tuple String (Array Int)) -> Map String (Set Int)
adjacency = Map.fromFoldable <<< map (map Set.fromFoldable)

assert :: String -> Boolean -> Effect Unit
assert name condition =
  if condition then log ("  PASS: " <> name)
  else log ("  FAIL: " <> name)

-- | The matching is valid: every pair is an edge, no left vertex twice.
valid :: Map String (Set Int) -> Map Int String -> Boolean
valid adj matching =
  Array.all
    (\(b /\ a) -> maybeElem b (Map.lookup a adj))
    (Map.toUnfoldable matching :: Array (Tuple Int String))
    && (Array.nub lefts == lefts)
  where
  lefts = Array.fromFoldable (Map.values matching)
  maybeElem b = case _ of
    Nothing -> false
    Just bs -> Set.member b bs

testMatching :: Effect Unit
testMatching = do
  log "Matching tests:"

  -- perfect matching exists
  let
    square = adjacency
      [ "a" /\ [ 1, 2 ]
      , "b" /\ [ 1 ]
      , "c" /\ [ 2, 3 ]
      ]
  let m1 = maximumBipartiteMatching square
  assert "3x3 with forced chain matches perfectly" (isPerfect square m1)
  assert "3x3 matching is valid" (valid square m1)
  assert "forced: b takes 1" (Map.lookup 1 m1 == Just "b")

  -- maximum but not perfect
  let
    starved = adjacency
      [ "a" /\ [ 1 ]
      , "b" /\ [ 1 ]
      , "c" /\ [ 1, 2 ]
      ]
  let m2 = maximumBipartiteMatching starved
  assert "starved graph matches only two" (Map.size m2 == 2)
  assert "starved matching is valid" (valid starved m2)

  -- augmenting path required: greedy would strand d
  let
    zigzag = adjacency
      [ "a" /\ [ 1, 2 ]
      , "b" /\ [ 2, 3 ]
      , "c" /\ [ 3, 4 ]
      , "d" /\ [ 4 ]
      ]
  let m3 = maximumBipartiteMatching zigzag
  assert "zigzag matches perfectly via augmentation" (isPerfect zigzag m3)
  assert "zigzag matching is valid" (valid zigzag m3)

  -- empty
  assert "empty graph gives empty matching"
    (Map.isEmpty (maximumBipartiteMatching (Map.empty :: Map String (Set Int))))
