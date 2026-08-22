-- | Tests for directed breadth-first search and the induced shortest-path tree.
module Test.InducedTree where

import Prelude

import Data.Graph.Algorithms (SimpleGraph, bfsTree, mkSimpleGraph, reverseGraph, treePath)
import Data.Graph.InducedTree (EdgeClass(..), classOf, depthOf, induce, pathFromRoot, returnCostOf, asymmetries)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

assert :: String -> Boolean -> Effect Unit
assert name condition =
  if condition
    then log $ "  PASS: " <> name
    else log $ "  FAIL: " <> name

assertEq :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEq name expected actual =
  if expected == actual
    then log $ "  PASS: " <> name
    else log $ "  FAIL: " <> name <> " - expected " <> show expected <> ", got " <> show actual

-- | A navigation-shaped graph with one of every edge class in it.
-- |
-- |     home ──▶ a ──▶ c ──▶ home     (and a ──▶ a)
-- |       └────▶ b ──▶ c
-- |     d ──▶ home                    (d is not reachable from home)
-- |     island                        (no edges at all)
navGraph :: SimpleGraph String
navGraph = mkSimpleGraph
  [ "home", "a", "b", "c", "d", "island" ]
  [ Tuple "home" "a"
  , Tuple "home" "b"
  , Tuple "a" "c"
  , Tuple "b" "c"
  , Tuple "c" "home"
  , Tuple "a" "a"
  , Tuple "d" "home"
  ]

testInducedTree :: Effect Unit
testInducedTree = do
  testDirectedBfs
  testInduce

testDirectedBfs :: Effect Unit
testDirectedBfs = do
  log "Directed BFS:"

  let chain = mkSimpleGraph [ 0, 1, 2, 3 ] [ Tuple 0 1, Tuple 1 2, Tuple 2 3 ]
  let fromZero = bfsTree 0 chain
  assertEq "chain: depths from 0" (Map.fromFoldable [ Tuple 0 0, Tuple 1 1, Tuple 2 2, Tuple 3 3 ]) fromZero.depth
  assertEq "chain: visit order" [ 0, 1, 2, 3 ] fromZero.order

  -- The whole point: an undirected BFS would reach 0 and 1 from here.
  let fromTwo = bfsTree 2 chain
  assertEq "chain: search respects edge direction" (Map.fromFoldable [ Tuple 2 0, Tuple 3 1 ]) fromTwo.depth

  let reversed = bfsTree 3 (reverseGraph chain)
  assertEq "chain: reversed search walks back" (Map.fromFoldable [ Tuple 3 0, Tuple 2 1, Tuple 1 2, Tuple 0 3 ]) reversed.depth

  assertEq "chain: path to 3" [ 0, 1, 2, 3 ] (treePath fromZero 3)
  assertEq "chain: no path to an unreached node" [] (treePath fromTwo 0)

  -- Depth must be the *shortest* distance, not the first path found.
  let diamond = mkSimpleGraph [ 0, 1, 2, 3 ] [ Tuple 0 1, Tuple 0 2, Tuple 1 3, Tuple 2 3, Tuple 0 3 ]
  assertEq "diamond: shortcut wins" (Just 1) (Map.lookup 3 (bfsTree 0 diamond).depth)

testInduce :: Effect Unit
testInduce = do
  log "Induced shortest-path tree:"

  let nav = induce "home" navGraph

  assertEq "depth of c" (Just 2) (depthOf nav "c")
  assertEq "depth of an unreachable node" Nothing (depthOf nav "d")
  assertEq "return cost of c" (Just 1) (returnCostOf nav "c")
  assertEq "return cost of a" (Just 2) (returnCostOf nav "a")

  assertEq "unreachable states" [ "d", "island" ] nav.unreachable
  assertEq "children of home" (Just [ "a", "b" ]) (Map.lookup "home" nav.children)

  assertEq "home -> a is a tree edge" (Just TreeEdge) (classOf nav "home" "a")
  assertEq "a -> c is a tree edge" (Just TreeEdge) (classOf nav "a" "c")
  assertEq "c -> home is a back edge" (Just BackEdge) (classOf nav "c" "home")
  assertEq "b -> c is a cross edge" (Just CrossEdge) (classOf nav "b" "c")
  assertEq "a -> a is a self edge" (Just SelfEdge) (classOf nav "a" "a")
  assertEq "d -> home leaves an unreachable state" (Just FromUnreachable) (classOf nav "d" "home")
  assertEq "an edge that is not there" Nothing (classOf nav "c" "b")

  assertEq "path from home to c" [ "home", "a", "c" ] (pathFromRoot nav "c")
  assertEq "no path to an unreachable state" [] (pathFromRoot nav "d")

  -- a and b are both one press away and two presses back.
  assertEq "asymmetries, worst first"
    [ { state: "a", inCost: 1, outCost: 2, excess: 1 }
    , { state: "b", inCost: 1, outCost: 2, excess: 1 }
    ]
    (asymmetries nav)
