module Test.Pathfinding where

import Prelude

import Data.Array as Array
import Data.Graph.Pathfinding (PathResult(..), bfs, dfs, shortestPath, bfsFrom, dfsFrom)
import Data.Graph.Types (NodeId(..), mkGraph, nodes, edges, neighbors, edgeWeight, updateWeight)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Console (log)

-- ===== Test utilities =====

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

-- ===== Shorthand =====

n :: String -> NodeId
n = NodeId

-- ===== Entry point =====

testPathfinding :: Effect Unit
testPathfinding = do
  testGraphTypes
  testShortestPath
  testBFS
  testDFS

-- ===== Graph Types (Data.Graph.Types) =====

testGraphTypes :: Effect Unit
testGraphTypes = do
  log "Graph types:"
  let g = mkGraph [n "A", n "B", n "C"]
        [ { from: n "A", to: n "B", weight: 1.0 }
        , { from: n "B", to: n "C", weight: 2.0 }
        ]
  assertEq "node count" 3 (Array.length $ nodes g)
  assertEq "edge count" 2 (Array.length $ edges g)

  -- Bidirectional adjacency
  assertEq "A has 1 neighbor" 1 (Array.length $ neighbors (n "A") g)
  assertEq "B has 2 neighbors" 2 (Array.length $ neighbors (n "B") g)
  assertEq "C has 1 neighbor" 1 (Array.length $ neighbors (n "C") g)

  -- Edge weights
  assertEq "A→B weight" (Just 1.0) (edgeWeight (n "A") (n "B") g)
  assertEq "B→A weight (reverse)" (Just 1.0) (edgeWeight (n "B") (n "A") g)
  assertEq "A→C weight (none)" Nothing (edgeWeight (n "A") (n "C") g)

  -- updateWeight
  let g' = updateWeight (n "A") (n "B") 5.0 g
  assertEq "updated A→B" (Just 5.0) (edgeWeight (n "A") (n "B") g')
  assertEq "updated B→A" (Just 5.0) (edgeWeight (n "B") (n "A") g')

-- ===== Shortest Path (Dijkstra) =====

testShortestPath :: Effect Unit
testShortestPath = do
  log "Shortest path:"
  -- A --1-- B --2-- C --3-- D
  --  \                     /
  --   --------10----------
  let g = mkGraph [n "A", n "B", n "C", n "D"]
        [ { from: n "A", to: n "B", weight: 1.0 }
        , { from: n "B", to: n "C", weight: 2.0 }
        , { from: n "C", to: n "D", weight: 3.0 }
        , { from: n "A", to: n "D", weight: 10.0 }
        ]

  -- Same node
  case shortestPath (n "A") (n "A") g of
    SameNode -> log "  PASS: same node"
    _ -> log "  FAIL: same node - expected SameNode"

  -- Shortest via B,C (cost 6) beats direct (cost 10)
  case shortestPath (n "A") (n "D") g of
    PathFound r -> do
      assertEq "A→D cost" 6.0 r.cost
      assertEq "A→D path" [n "A", n "B", n "C", n "D"] r.path
    _ -> log "  FAIL: A→D - expected PathFound"

  -- Direct neighbor
  case shortestPath (n "A") (n "B") g of
    PathFound r -> assertEq "A→B cost" 1.0 r.cost
    _ -> log "  FAIL: A→B - expected PathFound"

  -- Unreachable (disconnected graph)
  let disc = mkGraph [n "A", n "B", n "C"]
        [ { from: n "A", to: n "B", weight: 1.0 } ]
  case shortestPath (n "A") (n "C") disc of
    NoPath -> log "  PASS: no path to disconnected node"
    _ -> log "  FAIL: expected NoPath for disconnected node"

-- ===== BFS =====

testBFS :: Effect Unit
testBFS = do
  log "BFS:"
  -- A -- B -- D
  -- |
  -- C
  let g = mkGraph [n "A", n "B", n "C", n "D"]
        [ { from: n "A", to: n "B", weight: 1.0 }
        , { from: n "A", to: n "C", weight: 1.0 }
        , { from: n "B", to: n "D", weight: 1.0 }
        ]
  let result = bfs (n "A") g

  -- Depths (hop counts)
  assertEq "A depth 0" (Just 0.0) (Map.lookup (n "A") result.distances)
  assertEq "B depth 1" (Just 1.0) (Map.lookup (n "B") result.distances)
  assertEq "C depth 1" (Just 1.0) (Map.lookup (n "C") result.distances)
  assertEq "D depth 2" (Just 2.0) (Map.lookup (n "D") result.distances)

  -- All nodes visited
  assertEq "visited count" 4 (Array.length result.visited)

  -- bfsFrom returns same count
  assertEq "bfsFrom count" 4 (Array.length $ bfsFrom (n "A") g)

  -- BFS from leaf: reaches all (bidirectional graph)
  let fromD = bfs (n "D") g
  assertEq "from D: all visited" 4 (Array.length fromD.visited)
  assertEq "D self-depth" (Just 0.0) (Map.lookup (n "D") fromD.distances)
  assertEq "D→B depth 1" (Just 1.0) (Map.lookup (n "B") fromD.distances)

-- ===== DFS =====

testDFS :: Effect Unit
testDFS = do
  log "DFS:"
  let g = mkGraph [n "A", n "B", n "C"]
        [ { from: n "A", to: n "B", weight: 1.0 }
        , { from: n "B", to: n "C", weight: 1.0 }
        ]
  let result = dfs (n "A") g

  -- All nodes visited
  assertEq "visited count" 3 (Array.length result.visited)

  -- Start node is visited first
  assertEq "first visited" (Just (n "A")) (Array.head result.visited)

  -- All nodes have discovery times
  assert "A discovered" (isJust $ Map.lookup (n "A") result.distances)
  assert "B discovered" (isJust $ Map.lookup (n "B") result.distances)
  assert "C discovered" (isJust $ Map.lookup (n "C") result.distances)

  -- dfsFrom returns same nodes
  assertEq "dfsFrom count" 3 (Array.length $ dfsFrom (n "A") g)

  -- DFS on disconnected graph: only reaches connected component
  let disc = mkGraph [n "A", n "B", n "C"]
        [ { from: n "A", to: n "B", weight: 1.0 } ]
  assertEq "disconnected DFS" 2 (Array.length $ dfsFrom (n "A") disc)
