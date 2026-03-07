module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Graph.Decomposition as Dec
import Data.Graph.Weighted as WG
import Data.Graph.Weighted.DAG (DAGError(..))
import Data.Graph.Weighted.DAG as DAG
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)

-- Simple test utilities
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

-- Test data: A simple flow network
--     A --10--> B --5--> D
--      \               /
--       --3--> C --7--/
--
simpleFlowEdges :: Array { source :: String, target :: String, weight :: Number }
simpleFlowEdges =
  [ { source: "A", target: "B", weight: 10.0 }
  , { source: "A", target: "C", weight: 3.0 }
  , { source: "B", target: "D", weight: 5.0 }
  , { source: "C", target: "D", weight: 7.0 }
  ]

-- Linear chain: A -> B -> C -> D
linearChainEdges :: Array { source :: String, target :: String, weight :: Number }
linearChainEdges =
  [ { source: "A", target: "B", weight: 1.0 }
  , { source: "B", target: "C", weight: 1.0 }
  , { source: "C", target: "D", weight: 1.0 }
  ]

-- Graph with a cycle: A -> B -> C -> A
cyclicEdges :: Array { source :: String, target :: String, weight :: Number }
cyclicEdges =
  [ { source: "A", target: "B", weight: 1.0 }
  , { source: "B", target: "C", weight: 1.0 }
  , { source: "C", target: "A", weight: 1.0 }
  ]

main :: Effect Unit
main = do
  log "=== WeightedDigraph Tests ==="
  testWeightedDigraph
  log ""
  log "=== DAG Tests ==="
  testDAG
  log ""
  log "=== Decomposition Tests ==="
  testDecomposition
  log ""
  log "=== All tests completed ==="

testWeightedDigraph :: Effect Unit
testWeightedDigraph = do
  let graph = WG.fromEdges simpleFlowEdges

  log "Building graph from edges:"
  assertEq "node count" 4 (WG.nodeCount graph)
  assertEq "edge count" 4 (WG.edgeCount graph)

  log "Node membership:"
  assert "has node A" (WG.hasNode "A" graph)
  assert "has node D" (WG.hasNode "D" graph)
  assert "doesn't have node X" (not $ WG.hasNode "X" graph)

  log "Edge existence:"
  assert "has edge A->B" (WG.hasEdge "A" "B" graph)
  assert "has edge C->D" (WG.hasEdge "C" "D" graph)
  assert "doesn't have edge B->A (directed)" (not $ WG.hasEdge "B" "A" graph)

  log "Outgoing edges:"
  let outA = WG.outgoing "A" graph
  assertEq "A has 2 outgoing edges" 2 (Array.length outA)

  log "Incoming edges:"
  let inD = WG.incoming "D" graph
  assertEq "D has 2 incoming edges" 2 (Array.length inD)

  log "Sources and sinks:"
  let srcs = WG.sources graph
  let snks = WG.sinks graph
  assertEq "sources" ["A"] srcs
  assertEq "sinks" ["D"] snks

testDAG :: Effect Unit
testDAG = do
  log "Smart constructor validation:"

  -- Valid DAG
  let validGraph = WG.fromEdges simpleFlowEdges
  case DAG.fromWeightedDigraph validGraph of
    Right _ -> log "  PASS: accepts valid DAG"
    Left err -> log $ "  FAIL: rejected valid DAG with " <> show err

  -- Cyclic graph should be rejected
  let cyclicGraph = WG.fromEdges cyclicEdges
  case DAG.fromWeightedDigraph cyclicGraph of
    Left (CycleDetected _) -> log "  PASS: rejects cyclic graph"
    Right _ -> log "  FAIL: accepted cyclic graph"

  log "DAG operations (using simple flow network):"
  let dag = DAG.unsafeFromWeightedDigraph validGraph

  -- Sources and sinks
  assertEq "DAG sources" ["A"] (DAG.sources dag)
  assertEq "DAG sinks" ["D"] (DAG.sinks dag)

  log "Topological sort:"
  let topo = DAG.topologicalSort dag
  -- A must come before B and C, B and C must come before D
  let aIdx = Array.findIndex (_ == "A") topo
  let bIdx = Array.findIndex (_ == "B") topo
  let cIdx = Array.findIndex (_ == "C") topo
  let dIdx = Array.findIndex (_ == "D") topo
  case aIdx, bIdx, cIdx, dIdx of
    Just ai, Just bi, Just ci, Just di -> do
      assert "A before B" (ai < bi)
      assert "A before C" (ai < ci)
      assert "B before D" (bi < di)
      assert "C before D" (ci < di)
    _, _, _, _ -> log "  FAIL: missing nodes in topological sort"

  log "Depth computation (linear chain):"
  let linearGraph = WG.fromEdges linearChainEdges
  let linearDag = DAG.unsafeFromWeightedDigraph linearGraph
  let depthMap = DAG.depths linearDag
  assertEq "A depth" (Just 0) (Map.lookup "A" depthMap)
  assertEq "B depth" (Just 1) (Map.lookup "B" depthMap)
  assertEq "C depth" (Just 2) (Map.lookup "C" depthMap)
  assertEq "D depth" (Just 3) (Map.lookup "D" depthMap)

  log "Height computation (linear chain):"
  let heightMap = DAG.heights linearDag
  assertEq "A height" (Just 3) (Map.lookup "A" heightMap)
  assertEq "B height" (Just 2) (Map.lookup "B" heightMap)
  assertEq "C height" (Just 1) (Map.lookup "C" heightMap)
  assertEq "D height" (Just 0) (Map.lookup "D" heightMap)

  log "Depths on flow network:"
  let flowDepths = DAG.depths dag
  assertEq "A depth (flow)" (Just 0) (Map.lookup "A" flowDepths)
  assertEq "B depth (flow)" (Just 1) (Map.lookup "B" flowDepths)
  assertEq "C depth (flow)" (Just 1) (Map.lookup "C" flowDepths)
  assertEq "D depth (flow)" (Just 2) (Map.lookup "D" flowDepths)

testDecomposition :: Effect Unit
testDecomposition = do
  let g = Dec.testGraphs

  -- === Biconnected Components ===
  log "Biconnected components:"

  -- Diamond: 1 biconnected component (it's 2-connected)
  assertEq "diamond: 1 BCC" 1 (Array.length $ Dec.biconnectedComponents g.diamond)

  -- K5: 1 biconnected component
  assertEq "k5: 1 BCC" 1 (Array.length $ Dec.biconnectedComponents g.k5)

  -- Tree10: 9 BCCs (one per edge)
  assertEq "tree10: 9 BCCs" 9 (Array.length $ Dec.biconnectedComponents g.tree10)

  -- Cycle5: 1 BCC
  assertEq "cycle5: 1 BCC" 1 (Array.length $ Dec.biconnectedComponents g.cycle5)

  -- Barbell (two K4 + bridge): 3 BCCs (left K4, bridge, right K4)
  assertEq "barbell: 3 BCCs" 3 (Array.length $ Dec.biconnectedComponents g.barbell)

  -- Path4: 3 BCCs (one per edge)
  assertEq "path4: 3 BCCs" 3 (Array.length $ Dec.biconnectedComponents g.path4)

  -- Bowtie: 2 BCCs (two triangles sharing vertex C)
  assertEq "bowtie: 2 BCCs" 2 (Array.length $ Dec.biconnectedComponents g.bowtie)

  -- === Articulation Points ===
  log "Articulation points:"

  -- Diamond: no articulation points
  assertEq "diamond: 0 APs" 0 (Set.size $ Dec.articulationPoints g.diamond)

  -- K5: no articulation points
  assertEq "k5: 0 APs" 0 (Set.size $ Dec.articulationPoints g.k5)

  -- Star8: hub is the only articulation point
  let starAPs = Dec.articulationPoints g.star8
  assertEq "star8: 1 AP" 1 (Set.size starAPs)
  assert "star8: hub is AP" (Set.member "hub" starAPs)

  -- Bowtie: C is the only articulation point
  let bowtieAPs = Dec.articulationPoints g.bowtie
  assertEq "bowtie: 1 AP" 1 (Set.size bowtieAPs)
  assert "bowtie: C is AP" (Set.member "C" bowtieAPs)

  -- Barbell: D and E are articulation points
  let barbellAPs = Dec.articulationPoints g.barbell
  assertEq "barbell: 2 APs" 2 (Set.size barbellAPs)
  assert "barbell: D is AP" (Set.member "D" barbellAPs)
  assert "barbell: E is AP" (Set.member "E" barbellAPs)

  -- Path4: B and C are articulation points
  let pathAPs = Dec.articulationPoints g.path4
  assertEq "path4: 2 APs" 2 (Set.size pathAPs)
  assert "path4: B is AP" (Set.member "B" pathAPs)
  assert "path4: C is AP" (Set.member "C" pathAPs)

  -- Tree10: 8 internal nodes are articulation points (all except leaves)
  let treeAPs = Dec.articulationPoints g.tree10
  -- Leaves: H, I, J, G, D has child H so not leaf... let me count
  -- A has children B,C. B has children D,E. C has children F,G. D has H. E has I. F has J.
  -- Leaves: H, I, J, G. Internal: A, B, C, D, E, F = 6 APs
  -- Wait, G is a leaf (no children in a tree). F has child J. So internal nodes with >0 children in a tree are APs.
  -- Actually in a tree, every non-leaf node is an AP (except root if it has only 1 child, but root A has 2 children).
  -- A(2 children), B(2), C(2), D(1 child H), E(1 child I), F(1 child J)
  -- Root A has 2+ children → AP. Non-root with 1 child: D is AP because removing D disconnects H. Same for E, F.
  -- So APs = {A, B, C, D, E, F} = 6
  assertEq "tree10: internal nodes are APs" 6 (Set.size treeAPs)

  -- === Bridges ===
  log "Bridges:"

  -- Tree10: all 9 edges are bridges
  assertEq "tree10: 9 bridges" 9 (Array.length $ Dec.bridges g.tree10)

  -- K5: no bridges
  assertEq "k5: 0 bridges" 0 (Array.length $ Dec.bridges g.k5)

  -- Cycle5: no bridges
  assertEq "cycle5: 0 bridges" 0 (Array.length $ Dec.bridges g.cycle5)

  -- Barbell: 1 bridge (D-E)
  let barbellBr = Dec.bridges g.barbell
  assertEq "barbell: 1 bridge" 1 (Array.length barbellBr)

  -- Path4: 3 bridges (all edges)
  assertEq "path4: 3 bridges" 3 (Array.length $ Dec.bridges g.path4)

  -- === Bipartiteness ===
  log "Bipartiteness:"

  -- Even cycle → bipartite
  assert "cycle6: bipartite" (isRight $ Dec.detectBipartite g.cycle6)

  -- Odd cycle → not bipartite
  assert "cycle5: not bipartite" (isLeft $ Dec.detectBipartite g.cycle5)

  -- Tree → always bipartite
  assert "tree10: bipartite" (isRight $ Dec.detectBipartite g.tree10)

  -- K3,3 → bipartite
  case Dec.detectBipartite g.k33 of
    Right parts -> do
      assert "k33: bipartite" true
      assertEq "k33: partA size 3" 3 (Set.size parts.partA)
      assertEq "k33: partB size 3" 3 (Set.size parts.partB)
    Left _ -> assert "k33: should be bipartite" false

  -- K5 → not bipartite (odd clique)
  assert "k5: not bipartite" (isLeft $ Dec.detectBipartite g.k5)

  -- Diamond → bipartite (it's a 4-cycle)
  assert "diamond: bipartite" (isRight $ Dec.detectBipartite g.diamond)

  -- Path → bipartite
  assert "path4: bipartite" (isRight $ Dec.detectBipartite g.path4)

  -- === Block-Cut Tree ===
  log "Block-cut tree:"

  let barbellBCT = Dec.blockCutTree g.barbell
  assertEq "barbell BCT: 3 blocks" 3 (Array.length barbellBCT.blocks)
  assertEq "barbell BCT: 2 cut vertices" 2 (Set.size barbellBCT.cutVertices)
  -- Tree should have edges connecting blocks to cut vertices
  assert "barbell BCT: has tree edges" (Array.length barbellBCT.tree > 0)

  let bowtieBCT = Dec.blockCutTree g.bowtie
  assertEq "bowtie BCT: 2 blocks" 2 (Array.length bowtieBCT.blocks)
  assertEq "bowtie BCT: 1 cut vertex (C)" 1 (Set.size bowtieBCT.cutVertices)

  -- === Decomposition Metrics ===
  log "Decomposition metrics:"

  let treeMetrics = Dec.decompositionMetrics g.tree10
  assertEq "tree10 metrics: 9 BCCs" 9 treeMetrics.biconnectedComponentCount
  assert "tree10 metrics: isTree" treeMetrics.isTree
  assert "tree10 metrics: isBipartite" treeMetrics.isBipartite
  assert "tree10 metrics: treelikeness = 1.0" (treeMetrics.treelikeness == 1.0)

  let k5Metrics = Dec.decompositionMetrics g.k5
  assertEq "k5 metrics: 1 BCC" 1 k5Metrics.biconnectedComponentCount
  assertEq "k5 metrics: 0 bridges" 0 k5Metrics.bridgeCount
  assert "k5 metrics: not tree" (not k5Metrics.isTree)
  assert "k5 metrics: not bipartite" (not k5Metrics.isBipartite)
  assert "k5 metrics: treelikeness = 0.0" (k5Metrics.treelikeness == 0.0)

  where
  isRight :: forall a b. Either a b -> Boolean
  isRight (Right _) = true
  isRight _ = false

  isLeft :: forall a b. Either a b -> Boolean
  isLeft (Left _) = true
  isLeft _ = false
