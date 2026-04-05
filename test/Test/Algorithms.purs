module Test.Algorithms where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Ord (abs)
import Data.Graph.Algorithms (SimpleGraph, hasCycle, findCycle, isDAG, findBackEdges,
  reachableFrom, computeLayers, addLayers, transitiveReduction, getRemovedEdges, getAllEdges,
  connectedComponents, isConnected, stronglyConnectedComponents, isSCC,
  outDegreeCentrality, inDegreeCentrality,
  graphMetrics, density, averageDegree,
  pageRank, labelPropagation, modularity,
  taskNodesToSimpleGraph, simpleGraphToTaskNodes)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
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

assertApprox :: String -> Number -> Number -> Number -> Effect Unit
assertApprox name expected actual tolerance =
  if abs (expected - actual) < tolerance
    then log $ "  PASS: " <> name
    else log $ "  FAIL: " <> name <> " - expected ~" <> show expected <> ", got " <> show actual

-- ===== Graph builder =====

-- | Build a SimpleGraph Int from node list and edge tuples
mkSG :: Array Int -> Array (Tuple Int (Array Int)) -> SimpleGraph Int
mkSG ns edgeList =
  { nodes: ns
  , edges: Map.fromFoldable $ edgeList <#> \(Tuple src tgts) ->
      Tuple src (Set.fromFoldable tgts)
  }

-- ===== Cycle validation helper =====

-- | Verify a cycle returned by findCycle actually corresponds to real edges.
-- | A valid cycle [a, b, c, a] must have edges a→b, b→c, c→a and first == last.
isValidCycle :: SimpleGraph Int -> Array Int -> Boolean
isValidCycle graph cycle =
  let
    len = Array.length cycle
    wraps = len >= 2 && Array.head cycle == Array.last cycle
    pairs = Array.zip cycle (Array.drop 1 cycle)
    edgeExists (Tuple from to) = case Map.lookup from graph.edges of
      Nothing -> false
      Just targets -> Set.member to targets
  in wraps && all edgeExists pairs

-- ===== Entry point =====

testAlgorithms :: Effect Unit
testAlgorithms = do
  testReachability
  testCycleDetection
  testBackEdges
  testTopologicalLayers
  testTransitiveReduction
  testConnectedComponents
  testSCC
  testCentrality
  testGraphMetrics
  testPageRank
  testCommunityDetection
  testConversion

-- ===== Reachability =====

testReachability :: Effect Unit
testReachability = do
  log "Reachability:"
  -- Linear chain: 0→1→2→3
  let chain = mkSG [0,1,2,3] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [3]]
  assertEq "from 0 reaches all" (Set.fromFoldable [0,1,2,3]) (reachableFrom 0 chain)
  assertEq "from 2 reaches 2,3" (Set.fromFoldable [2,3]) (reachableFrom 2 chain)
  assertEq "from 3 reaches only 3" (Set.fromFoldable [3]) (reachableFrom 3 chain)

  -- Disconnected graph: 0→1, 2→3
  let disconnected = mkSG [0,1,2,3] [Tuple 0 [1], Tuple 2 [3]]
  assertEq "disconnected: from 0" (Set.fromFoldable [0,1]) (reachableFrom 0 disconnected)
  assert "disconnected: 2 not from 0" (not $ Set.member 2 $ reachableFrom 0 disconnected)

  -- Isolated node
  let isolated = mkSG [0] []
  assertEq "isolated: self only" (Set.fromFoldable [0]) (reachableFrom 0 isolated)

-- ===== Cycle Detection =====

testCycleDetection :: Effect Unit
testCycleDetection = do
  log "Cycle detection - basic:"

  -- DAG (diamond): no cycle
  let diamond = mkSG [0,1,2,3] [Tuple 0 [1,2], Tuple 1 [3], Tuple 2 [3]]
  assert "diamond: no cycle" (not $ hasCycle diamond)
  assert "diamond: isDAG" (isDAG diamond)
  assertEq "diamond: findCycle" Nothing (findCycle diamond)

  -- Simple 3-cycle: 0→1→2→0
  let tri = mkSG [0,1,2] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [0]]
  assert "triangle: has cycle" (hasCycle tri)
  assert "triangle: not DAG" (not $ isDAG tri)
  case findCycle tri of
    Nothing -> assert "triangle: found cycle" false
    Just c -> assert "triangle: valid cycle" (isValidCycle tri c)

  -- Self-loop: 0→0
  let selfLoop = mkSG [0,1] [Tuple 0 [0, 1]]
  assert "self-loop: has cycle" (hasCycle selfLoop)
  case findCycle selfLoop of
    Nothing -> assert "self-loop: found cycle" false
    Just c -> assert "self-loop: valid cycle" (isValidCycle selfLoop c)

  -- Single node, no edges
  assert "single: no cycle" (not $ hasCycle $ mkSG [0] [])

  -- Empty graph
  assert "empty: no cycle" (not $ hasCycle $ mkSG [] [])

  -- Long 5-cycle: 0→1→2→3→4→0
  let long = mkSG [0,1,2,3,4]
        [Tuple 0 [1], Tuple 1 [2], Tuple 2 [3], Tuple 3 [4], Tuple 4 [0]]
  assert "5-cycle: detected" (hasCycle long)
  case findCycle long of
    Nothing -> assert "5-cycle: found cycle" false
    Just c -> do
      assert "5-cycle: valid" (isValidCycle long c)
      assert "5-cycle: wraps" (Array.length c >= 2)

  -- Two-node cycle: 0↔1
  let twoNode = mkSG [0,1] [Tuple 0 [1], Tuple 1 [0]]
  assert "2-cycle: detected" (hasCycle twoNode)
  case findCycle twoNode of
    Nothing -> assert "2-cycle: found cycle" false
    Just c -> assert "2-cycle: valid" (isValidCycle twoNode c)

  log "Cycle detection - regression (phantom cycle bug):"
  -- This graph triggers the bug fixed in findCycle. The real cycle is
  -- 1→2→3→1. Without the early-return guard, after finding this cycle
  -- the DFS would continue processing node 5 (a sibling branch from 0).
  -- Node 5→{3,6}: since node 3 was left in the gray set (never moved
  -- to black because cleanup is skipped after cycle detection), the DFS
  -- would see 3 as a "back edge" and build phantom cycle [3,5,3],
  -- overwriting the correct cycle.
  let bugGraph = mkSG [0,1,2,3,5,6]
        [ Tuple 0 [1, 5]
        , Tuple 1 [2]
        , Tuple 2 [3]
        , Tuple 3 [1]
        , Tuple 5 [3, 6]
        ]
  assert "regression: has cycle" (hasCycle bugGraph)
  case findCycle bugGraph of
    Nothing -> assert "regression: found cycle" false
    Just c -> do
      assert "regression: valid cycle" (isValidCycle bugGraph c)
      -- The cycle must involve {1,2,3}, not phantom nodes 5 or 6
      assert "regression: no phantom nodes"
        (not (Array.elem 5 c) && not (Array.elem 6 c))

  -- Variant: external node 4→5→2 points into cycle 0→1→2→0
  let variant = mkSG [0,1,2,4,5]
        [ Tuple 0 [1]
        , Tuple 1 [2]
        , Tuple 2 [0]
        , Tuple 4 [5]
        , Tuple 5 [2]
        ]
  assert "variant: has cycle" (hasCycle variant)
  case findCycle variant of
    Nothing -> assert "variant: found cycle" false
    Just c -> assert "variant: valid cycle" (isValidCycle variant c)

  -- Disconnected graph: one component has cycle, other doesn't
  let halfCyclic = mkSG [0,1,2,3,4]
        [ Tuple 0 [1], Tuple 1 [2], Tuple 2 [0]  -- cycle
        , Tuple 3 [4]                              -- no cycle
        ]
  assert "half-cyclic: has cycle" (hasCycle halfCyclic)
  case findCycle halfCyclic of
    Nothing -> assert "half-cyclic: found cycle" false
    Just c -> do
      assert "half-cyclic: valid cycle" (isValidCycle halfCyclic c)
      assert "half-cyclic: cycle in {0,1,2}"
        (not (Array.elem 3 c) && not (Array.elem 4 c))

-- ===== Back-Edge Detection =====

testBackEdges :: Effect Unit
testBackEdges = do
  log "Back-edge detection:"

  -- Acyclic graph: no back-edges
  let dag = mkSG [0,1,2,3] [Tuple 0 [1,2], Tuple 1 [3], Tuple 2 [3]]
  assertEq "DAG: no back-edges" Set.empty (findBackEdges dag)

  -- Simple 3-cycle: 0→1→2→0 — one back-edge
  let tri = mkSG [0,1,2] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [0]]
  let triBack = findBackEdges tri
  assertEq "triangle: 1 back-edge" 1 (Set.size triBack)
  assert "triangle: 2→0 is back-edge" (Set.member (Tuple 2 0) triBack)

  -- Interior cycle: 0→1→2→3, 2→1 — back-edge is 2→1
  let interior = mkSG [0,1,2,3] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [1, 3]]
  let intBack = findBackEdges interior
  assertEq "interior: 1 back-edge" 1 (Set.size intBack)
  assert "interior: 2→1 is back-edge" (Set.member (Tuple 2 1) intBack)

  -- Multiple cycles sharing nodes: 0→1→2→3, 2→1, 3→0
  let multi = mkSG [0,1,2,3] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [1, 3], Tuple 3 [0]]
  let multiBack = findBackEdges multi
  assertEq "multiple: 2 back-edges" 2 (Set.size multiBack)
  assert "multiple: 2→1" (Set.member (Tuple 2 1) multiBack)
  assert "multiple: 3→0" (Set.member (Tuple 3 0) multiBack)

  -- Self-loop: 0→0
  let selfLoop = mkSG [0] [Tuple 0 [0]]
  assertEq "self-loop: 1 back-edge" 1 (Set.size $ findBackEdges selfLoop)
  assert "self-loop: 0→0" (Set.member (Tuple 0 0) $ findBackEdges selfLoop)

  -- Removing back-edges makes graph acyclic (feedback arc set property)
  let withCycles = mkSG [0,1,2,3,4]
        [ Tuple 0 [1], Tuple 1 [2], Tuple 2 [0]  -- cycle 0→1→2→0
        , Tuple 3 [4], Tuple 4 [3]                -- cycle 3↔4
        ]
  let backEdges = findBackEdges withCycles
  -- Build graph with back-edges removed
  let cleaned =
        { nodes: withCycles.nodes
        , edges: foldl (\edges (Tuple src tgt) ->
            Map.alter (map (Set.delete tgt)) src edges
          ) withCycles.edges (Set.toUnfoldable backEdges :: Array (Tuple Int Int))
        }
  assert "feedback arc set: result is DAG" (isDAG cleaned)

  -- Regression: graph that triggered phantom cycles in findCycle.
  -- Nodes: I(0), R(1), S(2), Bp(3), P(4), Ref(5), O(6)
  -- Only cycle is S→R (2→1), with R→S already present as 1→2.
  let regression = mkSG [0,1,2,3,4,5,6]
        [ Tuple 0 [1]      -- I→R
        , Tuple 1 [2, 3]   -- R→S, R→Bp
        , Tuple 2 [4, 1]   -- S→P, S→R (back-edge)
        , Tuple 4 [5]      -- P→Ref
        , Tuple 5 [6, 3]   -- Ref→O, Ref→Bp
        ]
  let regBack = findBackEdges regression
  assertEq "regression: 1 back-edge" 1 (Set.size regBack)
  assert "regression: 2→1 (S→R)" (Set.member (Tuple 2 1) regBack)

-- ===== Topological Layers =====

testTopologicalLayers :: Effect Unit
testTopologicalLayers = do
  log "Topological layers:"

  -- Diamond: A→{B,C}, B→D, C→D
  -- Layer 0: D (no outgoing), Layer 1: B,C, Layer 2: A
  let tasks =
        [ { id: "A", depends: ["B", "C"] }
        , { id: "B", depends: ["D"] }
        , { id: "C", depends: ["D"] }
        , { id: "D", depends: [] :: Array String }
        ]
  let layers = computeLayers tasks
  assertEq "D layer" (Just 0) (Map.lookup "D" layers)
  assertEq "B layer" (Just 1) (Map.lookup "B" layers)
  assertEq "C layer" (Just 1) (Map.lookup "C" layers)
  assertEq "A layer" (Just 2) (Map.lookup "A" layers)

  -- addLayers preserves computed layers
  let layered = addLayers tasks
  case Array.find (\n -> n.id == "A") layered, Array.find (\n -> n.id == "D") layered of
    Just a, Just d -> do
      assertEq "addLayers A" 2 a.layer
      assertEq "addLayers D" 0 d.layer
    _, _ -> assert "addLayers: nodes found" false

-- ===== Transitive Reduction =====

testTransitiveReduction :: Effect Unit
testTransitiveReduction = do
  log "Transitive reduction:"

  -- Diamond: 0→{1,2}, 1→{2} — edge 0→2 is transitive (via 0→1→2)
  let diamond = mkSG [0,1,2] [Tuple 0 [1,2], Tuple 1 [2]]
  let reduced = transitiveReduction diamond
  let redEdges = getAllEdges reduced
  assert "diamond: 0→1 kept" (Array.elem (Tuple 0 1) redEdges)
  assert "diamond: 1→2 kept" (Array.elem (Tuple 1 2) redEdges)
  assert "diamond: 0→2 removed" (not $ Array.elem (Tuple 0 2) redEdges)

  -- Linear chain: nothing to remove
  let chain = mkSG [0,1,2] [Tuple 0 [1], Tuple 1 [2]]
  assertEq "chain: no removal" 2 (Array.length $ getAllEdges $ transitiveReduction chain)

  -- Complete DAG: 0→{1,2,3}, 1→{2,3}, 2→{3} — reduces to 0→1, 1→2, 2→3
  let complete = mkSG [0,1,2,3]
        [Tuple 0 [1,2,3], Tuple 1 [2,3], Tuple 2 [3]]
  let compReduced = transitiveReduction complete
  let compEdges = getAllEdges compReduced
  assertEq "complete: 3 edges remain" 3 (Array.length compEdges)
  assert "complete: 0→1 kept" (Array.elem (Tuple 0 1) compEdges)
  assert "complete: 1→2 kept" (Array.elem (Tuple 1 2) compEdges)
  assert "complete: 2→3 kept" (Array.elem (Tuple 2 3) compEdges)

  -- getRemovedEdges
  let removed = getRemovedEdges complete compReduced
  assertEq "complete: 3 edges removed" 3 (Array.length removed)

-- ===== Connected Components =====

testConnectedComponents :: Effect Unit
testConnectedComponents = do
  log "Connected components:"

  -- Single component (undirected view: 0—1—2)
  let connected = mkSG [0,1,2] [Tuple 0 [1], Tuple 1 [2]]
  assertEq "single: 1 component" 1 (Array.length $ connectedComponents connected)
  assert "single: isConnected" (isConnected connected)

  -- Two disconnected components
  let twoComps = mkSG [0,1,2,3] [Tuple 0 [1], Tuple 2 [3]]
  assertEq "two: 2 components" 2 (Array.length $ connectedComponents twoComps)
  assert "two: not connected" (not $ isConnected twoComps)

  -- All isolated nodes
  let isolated = mkSG [0,1,2] []
  assertEq "isolated: 3 components" 3 (Array.length $ connectedComponents isolated)

  -- Empty graph
  assert "empty: connected" (isConnected $ mkSG [] [])

  -- Single node
  assert "single node: connected" (isConnected $ mkSG [0] [])

-- ===== Strongly Connected Components =====

testSCC :: Effect Unit
testSCC = do
  log "Strongly connected components:"

  -- Two SCCs linked by cross-edge: {0,1,2} and {3,4,5}
  let twoSCCs = mkSG [0,1,2,3,4,5]
        [ Tuple 0 [1], Tuple 1 [2], Tuple 2 [0, 3]
        , Tuple 3 [4], Tuple 4 [5], Tuple 5 [3]
        ]
  let sccs = stronglyConnectedComponents twoSCCs
  assertEq "twoSCCs: 2 components" 2 (Array.length sccs)
  assert "twoSCCs: {0,1,2}" (Array.elem (Set.fromFoldable [0,1,2]) sccs)
  assert "twoSCCs: {3,4,5}" (Array.elem (Set.fromFoldable [3,4,5]) sccs)

  -- DAG: each node is its own SCC
  let dag = mkSG [0,1,2,3] [Tuple 0 [1,2], Tuple 1 [3], Tuple 2 [3]]
  let dagSCCs = stronglyConnectedComponents dag
  assertEq "DAG: 4 SCCs" 4 (Array.length dagSCCs)
  assert "DAG: all singletons" (all (\s -> Set.size s == 1) dagSCCs)

  -- Single SCC (full cycle)
  let cycle = mkSG [0,1,2] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [0]]
  assert "cycle: isSCC" (isSCC cycle)
  assert "DAG: not isSCC" (not $ isSCC dag)

-- ===== Centrality =====

testCentrality :: Effect Unit
testCentrality = do
  log "Centrality:"

  -- Star: 0→{1,2,3}. 4 nodes, n-1 = 3
  let star = mkSG [0,1,2,3] [Tuple 0 [1,2,3]]
  let outDeg = outDegreeCentrality star
  let inDeg = inDegreeCentrality star

  -- Node 0: out-degree 3, centrality = 3/3 = 1.0
  assertApprox "out-degree 0" 1.0 (fromMaybe (-1.0) $ Map.lookup 0 outDeg) 0.001
  -- Node 1: out-degree 0, centrality = 0.0
  assertApprox "out-degree 1" 0.0 (fromMaybe (-1.0) $ Map.lookup 1 outDeg) 0.001
  -- Node 0: in-degree 0, centrality = 0.0
  assertApprox "in-degree 0" 0.0 (fromMaybe (-1.0) $ Map.lookup 0 inDeg) 0.001
  -- Node 1: in-degree 1, centrality = 1/3 ≈ 0.333
  assertApprox "in-degree 1" 0.333 (fromMaybe (-1.0) $ Map.lookup 1 inDeg) 0.01

-- ===== Graph Metrics =====

testGraphMetrics :: Effect Unit
testGraphMetrics = do
  log "Graph metrics:"

  -- Complete directed K3: 6 edges / 6 max = density 1.0
  let k3 = mkSG [0,1,2] [Tuple 0 [1,2], Tuple 1 [0,2], Tuple 2 [0,1]]
  assertApprox "K3 density" 1.0 (density k3) 0.001

  -- No edges: density 0.0
  assertApprox "no edges density" 0.0 (density $ mkSG [0,1,2] []) 0.001

  -- Star 0→{1,2,3}: 3 edges, 4 nodes, avgDeg = 2*3/4 = 1.5
  let star = mkSG [0,1,2,3] [Tuple 0 [1,2,3]]
  assertApprox "star avgDeg" 1.5 (averageDegree star) 0.001

  let m = graphMetrics star
  assertEq "star nodes" 4 m.nodeCount
  assertEq "star edges" 3 m.edgeCount
  assert "star isDAG" m.isDAG
  assert "star isConnected" m.isConnected

-- ===== PageRank =====

testPageRank :: Effect Unit
testPageRank = do
  log "PageRank:"

  -- Cycle with extra edge: 0→1, 1→2, 2→{0,1}
  -- No dangling nodes, so rank is conserved.
  -- Node 1 has 2 incoming edges (from 0 and 2), so should rank highest.
  let graph = mkSG [0,1,2] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [0, 1]]
  let ranks = pageRank graph

  -- Scores should sum to approximately 1.0 (no dangling nodes)
  let totalRank = foldl (+) 0.0 (Map.values ranks)
  assertApprox "sum ≈ 1.0" 1.0 totalRank 0.01

  -- Node 1 should have highest rank (most incoming edges)
  let r0 = fromMaybe 0.0 $ Map.lookup 0 ranks
  let r1 = fromMaybe 0.0 $ Map.lookup 1 ranks
  let r2 = fromMaybe 0.0 $ Map.lookup 2 ranks
  assert "node 1 highest rank" (r1 > r0 && r1 > r2)

  -- Symmetric cycle: all ranks equal
  let symCycle = mkSG [0,1,2] [Tuple 0 [1], Tuple 1 [2], Tuple 2 [0]]
  let symRanks = pageRank symCycle
  let sr0 = fromMaybe 0.0 $ Map.lookup 0 symRanks
  let sr1 = fromMaybe 0.0 $ Map.lookup 1 symRanks
  assertApprox "symmetric: equal ranks" sr0 sr1 0.001

-- ===== Community Detection =====

testCommunityDetection :: Effect Unit
testCommunityDetection = do
  log "Community detection:"

  -- Two disconnected cliques: {0↔1} and {2↔3}
  let disconn = mkSG [0,1,2,3] [Tuple 0 [1], Tuple 1 [0], Tuple 2 [3], Tuple 3 [2]]
  let comms = labelPropagation disconn
  -- Same-component nodes should share a label
  assertEq "0,1 same community" (Map.lookup 0 comms) (Map.lookup 1 comms)
  assertEq "2,3 same community" (Map.lookup 2 comms) (Map.lookup 3 comms)
  -- Different components should have different labels
  assert "communities differ" (Map.lookup 0 comms /= Map.lookup 2 comms)

  -- Modularity should be positive for a good partition
  let m = modularity disconn
        (Map.fromFoldable [Tuple 0 0, Tuple 1 0, Tuple 2 2, Tuple 3 2])
  assert "modularity > 0" (m > 0.0)

-- ===== Conversion =====

testConversion :: Effect Unit
testConversion = do
  log "Conversion:"

  let original = mkSG [0,1,2] [Tuple 0 [1,2], Tuple 1 [2]]
  let roundTripped = taskNodesToSimpleGraph $ simpleGraphToTaskNodes original
  assertEq "round-trip nodes" (Array.sort original.nodes) (Array.sort roundTripped.nodes)
  -- Compare via getAllEdges (ignores empty-set entries added by conversion)
  assertEq "round-trip edges"
    (Array.sort $ getAllEdges original)
    (Array.sort $ getAllEdges roundTripped)
