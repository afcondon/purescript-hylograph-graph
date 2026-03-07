-- | Graph decomposition algorithms for structural analysis.
-- |
-- | Algorithms for identifying structural features that suggest
-- | visualization strategies:
-- |
-- | - **Biconnected Components**: maximal 2-connected subgraphs
-- | - **Articulation Points**: vertices whose removal disconnects the graph
-- | - **Bridges**: edges whose removal disconnects the graph
-- | - **Bipartiteness**: 2-colorability test with witness
-- | - **Block-Cut Tree**: tree of blocks + cut vertices (the chimera skeleton)
-- |
-- | All algorithms work with `SimpleGraph` (undirected interpretation).
-- | Complexity is O(V + E) unless noted otherwise.
module Data.Graph.Decomposition
  ( -- Biconnected components
    biconnectedComponents
  , articulationPoints
  , bridges
    -- Bipartiteness
  , detectBipartite
    -- Block-cut tree
  , BlockCutNode(..)
  , blockCutTree
    -- Extended metrics
  , DecompositionMetrics
  , decompositionMetrics
    -- Test graph constructors
  , TestGraphs
  , testGraphs
  ) where

import Prelude

import Data.Array (foldl, length, snoc, uncons)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Graph.Algorithms (SimpleGraph)

-- =============================================================================
-- Undirected adjacency helper
-- =============================================================================

-- | Build symmetric adjacency map (treats directed graph as undirected)
undirectedAdj :: forall node. Ord node => SimpleGraph node -> Map node (Set node)
undirectedAdj graph =
  foldl (\acc (Tuple src targets) ->
    let
      acc' = Map.alter (Just <<< Set.union targets <<< fromMaybe Set.empty) src acc
      acc'' = foldl (\a tgt ->
        Map.alter (Just <<< Set.insert src <<< fromMaybe Set.empty) tgt a
      ) acc' (Set.toUnfoldable targets :: Array node)
    in acc''
  ) Map.empty (Map.toUnfoldable graph.edges :: Array (Tuple node (Set node)))

neighbors :: forall node. Ord node => node -> Map node (Set node) -> Array node
neighbors v adj = case Map.lookup v adj of
  Nothing -> []
  Just s -> Set.toUnfoldable s

-- =============================================================================
-- Biconnected Components + Articulation Points (Tarjan's bridge-finding DFS)
-- =============================================================================

type BCCState node =
  { timer :: Int
  , disc :: Map node Int
  , low :: Map node Int
  , parent :: Map node node
  , stack :: Array (Tuple node node)  -- edge stack
  , components :: Array (Set (Tuple node node))  -- biconnected components as edge sets
  , aps :: Set node  -- articulation points
  }

-- | Find all biconnected components (as sets of edges).
-- | Each component is a maximal subgraph with no cut vertex.
biconnectedComponents :: forall node. Ord node => SimpleGraph node -> Array (Set (Tuple node node))
biconnectedComponents graph =
  let adj = undirectedAdj graph
      result = foldl (\state node ->
        if Map.member node state.disc
        then state
        else bccDFS adj node state
      ) initState graph.nodes
  in result.components
  where
  initState :: BCCState node
  initState =
    { timer: 0
    , disc: Map.empty
    , low: Map.empty
    , parent: Map.empty
    , stack: []
    , components: []
    , aps: Set.empty
    }

-- | Find all articulation points (cut vertices).
-- | A vertex is an articulation point if removing it disconnects the graph.
articulationPoints :: forall node. Ord node => SimpleGraph node -> Set node
articulationPoints graph =
  let adj = undirectedAdj graph
      result = foldl (\state node ->
        if Map.member node state.disc
        then state
        else bccDFS adj node state
      ) initState graph.nodes
  in result.aps
  where
  initState :: BCCState node
  initState =
    { timer: 0
    , disc: Map.empty
    , low: Map.empty
    , parent: Map.empty
    , stack: []
    , components: []
    , aps: Set.empty
    }

-- | Core DFS for biconnected components and articulation points
bccDFS :: forall node. Ord node => Map node (Set node) -> node -> BCCState node -> BCCState node
bccDFS adj u state0 =
  let
    state1 = state0
      { timer = state0.timer + 1
      , disc = Map.insert u state0.timer state0.disc
      , low = Map.insert u state0.timer state0.low
      }
    nbrs = neighbors u adj
    childCount = 0
    isRoot = not (Map.member u state0.parent)
  in
    processNeighbors adj u nbrs isRoot childCount state1

processNeighbors :: forall node. Ord node =>
  Map node (Set node) -> node -> Array node -> Boolean -> Int -> BCCState node -> BCCState node
processNeighbors adj u nbrs isRoot childCount state =
  case uncons nbrs of
    Nothing ->
      -- All neighbors processed. If root with 2+ children, it's an AP
      if isRoot && childCount >= 2
      then state { aps = Set.insert u state.aps }
      else state
    Just { head: v, tail: rest } ->
      let uDisc = fromMaybe 0 $ Map.lookup u state.disc
      in case Map.lookup v state.disc of
        Nothing ->
          -- Tree edge: v not yet visited
          let
            state' = state
              { parent = Map.insert v u state.parent
              , stack = snoc state.stack (Tuple u v)
              }
            state'' = bccDFS adj v state'
            uLow = fromMaybe 0 $ Map.lookup u state''.low
            vLow = fromMaybe 0 $ Map.lookup v state''.low
            state3 = state'' { low = Map.insert u (min uLow vLow) state''.low }
            -- Check articulation point condition
            state4 =
              if not isRoot && vLow >= uDisc
              then
                -- u is an articulation point; pop edges until (u,v)
                let popped = popEdgesUntil state3.stack (Tuple u v)
                in state3
                    { aps = Set.insert u state3.aps
                    , stack = popped.remaining
                    , components = snoc state3.components popped.component
                    }
              else if isRoot
              then
                -- For root, pop each child's subtree as a component
                let popped = popEdgesUntil state3.stack (Tuple u v)
                in state3
                    { stack = popped.remaining
                    , components = snoc state3.components popped.component
                    }
              else state3
          in processNeighbors adj u rest isRoot (childCount + 1) state4
        Just vDisc ->
          -- Back edge (only if v is not parent)
          let parentOfU = Map.lookup u state.parent
          in if Just v /= parentOfU && vDisc < uDisc
             then
               let
                 uLow = fromMaybe 0 $ Map.lookup u state.low
                 state' = state
                   { low = Map.insert u (min uLow vDisc) state.low
                   , stack = snoc state.stack (Tuple u v)
                   }
               in processNeighbors adj u rest isRoot childCount state'
             else
               processNeighbors adj u rest isRoot childCount state

-- | Pop edges from stack until the target edge is found
popEdgesUntil :: forall node. Ord node =>
  Array (Tuple node node) -> Tuple node node -> { component :: Set (Tuple node node), remaining :: Array (Tuple node node) }
popEdgesUntil stack target = go stack Set.empty
  where
  go stk acc = case Array.last stk of
    Nothing -> { component: acc, remaining: [] }
    Just edge ->
      let stk' = fromMaybe [] (Array.init stk)
          acc' = Set.insert edge acc
      in if edge == target
         then { component: acc', remaining: stk' }
         else go stk' acc'

-- =============================================================================
-- Bridge Detection
-- =============================================================================

-- | Find all bridges (cut edges).
-- | An edge is a bridge iff it is the sole edge in a biconnected component.
bridges :: forall node. Ord node => SimpleGraph node -> Array (Tuple node node)
bridges graph =
  let comps = biconnectedComponents graph
  in Array.concatMap (\comp ->
    if Set.size comp == 1
    then Set.toUnfoldable comp
    else []
  ) comps

-- =============================================================================
-- Bipartiteness Testing (BFS 2-coloring)
-- =============================================================================

-- | Test if a graph is bipartite.
-- | Returns `Right (partA, partB)` if bipartite, or `Left oddCycle` if not.
detectBipartite :: forall node. Ord node => SimpleGraph node ->
  Either (Array node) { partA :: Set node, partB :: Set node }
detectBipartite graph =
  let adj = undirectedAdj graph
  in foldl (\result node ->
    case result of
      Left _ -> result  -- already found odd cycle
      Right parts ->
        if Map.member node parts.colors
        then result  -- already colored
        else
          case bfsColor adj node parts.colors of
            Left cycle -> Left cycle
            Right colors' ->
              let
                partA' = foldl (\s (Tuple n c) -> if c then Set.insert n s else s) parts.partA
                           (Map.toUnfoldable colors' :: Array (Tuple node Boolean))
                partB' = foldl (\s (Tuple n c) -> if not c then Set.insert n s else s) parts.partB
                           (Map.toUnfoldable colors' :: Array (Tuple node Boolean))
              in Right { colors: colors', partA: partA', partB: partB' }
  ) (Right { colors: Map.empty, partA: Set.empty, partB: Set.empty }) graph.nodes
  <#> \parts -> { partA: parts.partA, partB: parts.partB }

-- BFS 2-coloring from a start node
bfsColor :: forall node. Ord node =>
  Map node (Set node) -> node -> Map node Boolean -> Either (Array node) (Map node Boolean)
bfsColor adj start colors0 =
  go [start] (Map.insert start true colors0)
  where
  go queue colors =
    case uncons queue of
      Nothing -> Right colors
      Just { head: v, tail: rest } ->
        let
          vColor = fromMaybe true $ Map.lookup v colors
          nbrs = neighbors v adj
        in processColorNeighbors adj v vColor nbrs rest colors

processColorNeighbors :: forall node. Ord node =>
  Map node (Set node) -> node -> Boolean -> Array node -> Array node -> Map node Boolean -> Either (Array node) (Map node Boolean)
processColorNeighbors adj v vColor nbrs queue colors =
  case uncons nbrs of
    Nothing -> go queue colors
      where
      go q c = case uncons q of
        Nothing -> Right c
        Just { head: u, tail: rest } ->
          let
            uColor = fromMaybe true $ Map.lookup u c
            ns = neighbors u adj
          in processColorNeighbors adj u uColor ns rest c
    Just { head: w, tail: rest } ->
      case Map.lookup w colors of
        Nothing ->
          -- Not yet colored — assign opposite color
          processColorNeighbors adj v vColor rest (snoc queue w) (Map.insert w (not vColor) colors)
        Just wColor ->
          if wColor == vColor
          then Left [v, w]  -- Same color = odd cycle found
          else processColorNeighbors adj v vColor rest queue colors

-- =============================================================================
-- Block-Cut Tree
-- =============================================================================

-- | A node in the block-cut tree
data BlockCutNode node
  = Block Int (Set node)   -- ^ A biconnected component (index + member nodes)
  | CutVertex node         -- ^ An articulation point

-- | Build the block-cut tree from a graph.
-- | The tree's internal structure mirrors how the graph decomposes:
-- | blocks (dense subgraphs) connected at cut vertices (junction points).
blockCutTree :: forall node. Ord node => SimpleGraph node ->
  { blocks :: Array (Set node)
  , cutVertices :: Set node
  , tree :: Array { from :: Int, to :: Int }  -- adjacency between block/cut-vertex indices
  }
blockCutTree graph =
  let
    edgeComps = biconnectedComponents graph
    aps = articulationPoints graph

    -- Convert edge-set components to node-set components
    blocks = map (\edgeSet ->
      foldl (\acc (Tuple a b) -> Set.insert a (Set.insert b acc)) Set.empty
        (Set.toUnfoldable edgeSet :: Array (Tuple node node))
    ) edgeComps

    -- Build tree: blocks are indexed 0..n-1, cut vertices get indices n..n+m-1
    nBlocks = length blocks
    apArray = Set.toUnfoldable aps :: Array node
    apIndices = foldl (\m (Tuple i ap) -> Map.insert ap (nBlocks + i) m) Map.empty
                  (Array.mapWithIndex (\i ap -> Tuple i ap) apArray)

    -- Connect: for each cut vertex, find which blocks contain it
    treeEdges = Array.concatMap (\(Tuple apNode apIdx) ->
      Array.mapWithIndex (\blockIdx block ->
        if Set.member apNode block
        then [{ from: blockIdx, to: apIdx }]
        else []
      ) blocks # Array.concat
    ) (Map.toUnfoldable apIndices :: Array (Tuple node Int))
  in
    { blocks, cutVertices: aps, tree: treeEdges }

-- =============================================================================
-- Decomposition Metrics
-- =============================================================================

type DecompositionMetrics =
  { biconnectedComponentCount :: Int
  , articulationPointCount :: Int
  , bridgeCount :: Int
  , isBipartite :: Boolean
  , isTree :: Boolean
  , maxBlockSize :: Int
  , treelikeness :: Number  -- bridges / edges (1.0 = tree, 0.0 = fully 2-connected)
  }

-- | Compute structural decomposition metrics for a graph
decompositionMetrics :: forall node. Ord node => SimpleGraph node -> DecompositionMetrics
decompositionMetrics graph =
  let
    comps = biconnectedComponents graph
    aps = articulationPoints graph
    br = bridges graph
    bip = case detectBipartite graph of
      Left _ -> false
      Right _ -> true
    -- Undirected edge count: each edge stored in both directions, so divide by 2
    edgeCount = foldl (\acc (Tuple _ targets) -> acc + Set.size targets) 0
                  (Map.toUnfoldable graph.edges :: Array (Tuple node (Set node))) / 2
    nodeCount = length graph.nodes
    blockSizes = map (\edgeSet ->
      Set.size $ foldl (\acc (Tuple a b) -> Set.insert a (Set.insert b acc)) Set.empty
        (Set.toUnfoldable edgeSet :: Array (Tuple node node))
    ) comps
    maxBlock = fromMaybe 0 $ Array.last $ Array.sort blockSizes
  in
    { biconnectedComponentCount: length comps
    , articulationPointCount: Set.size aps
    , bridgeCount: length br
    , isBipartite: bip
    , isTree: edgeCount == nodeCount - 1 && nodeCount > 0
    , maxBlockSize: maxBlock
    , treelikeness: if edgeCount == 0 then 1.0
                    else Int.toNumber (length br) / Int.toNumber edgeCount
    }

-- =============================================================================
-- Test Graph Suite
-- =============================================================================

type TestGraphs =
  { tree10 :: SimpleGraph String
  , cycle5 :: SimpleGraph String
  , cycle6 :: SimpleGraph String
  , barbell :: SimpleGraph String
  , star8 :: SimpleGraph String
  , k5 :: SimpleGraph String
  , k33 :: SimpleGraph String
  , diamond :: SimpleGraph String
  , path4 :: SimpleGraph String
  , bowtie :: SimpleGraph String
  }

-- | Collection of canonical test graphs with known properties
testGraphs :: TestGraphs
testGraphs =
  { tree10: mkUndirected
      ["A","B","C","D","E","F","G","H","I","J"]
      [t "A" "B", t "A" "C", t "B" "D", t "B" "E", t "C" "F", t "C" "G", t "D" "H", t "E" "I", t "F" "J"]
  , cycle5: mkUndirected
      ["A","B","C","D","E"]
      [t "A" "B", t "B" "C", t "C" "D", t "D" "E", t "E" "A"]
  , cycle6: mkUndirected
      ["A","B","C","D","E","F"]
      [t "A" "B", t "B" "C", t "C" "D", t "D" "E", t "E" "F", t "F" "A"]
  , barbell: mkUndirected
      ["A","B","C","D","E","F","G","H"]
      -- Left K4
      [t "A" "B", t "A" "C", t "A" "D", t "B" "C", t "B" "D", t "C" "D"
      -- Bridge
      , t "D" "E"
      -- Right K4
      , t "E" "F", t "E" "G", t "E" "H", t "F" "G", t "F" "H", t "G" "H"]
  , star8: mkUndirected
      ["hub","A","B","C","D","E","F","G","H"]
      [t "hub" "A", t "hub" "B", t "hub" "C", t "hub" "D", t "hub" "E", t "hub" "F", t "hub" "G", t "hub" "H"]
  , k5: mkUndirected
      ["A","B","C","D","E"]
      [t "A" "B", t "A" "C", t "A" "D", t "A" "E", t "B" "C", t "B" "D", t "B" "E", t "C" "D", t "C" "E", t "D" "E"]
  , k33: mkUndirected
      ["A","B","C","X","Y","Z"]
      [t "A" "X", t "A" "Y", t "A" "Z", t "B" "X", t "B" "Y", t "B" "Z", t "C" "X", t "C" "Y", t "C" "Z"]
  , diamond: mkUndirected
      ["A","B","C","D"]
      [t "A" "B", t "A" "C", t "B" "D", t "C" "D"]
  , path4: mkUndirected
      ["A","B","C","D"]
      [t "A" "B", t "B" "C", t "C" "D"]
  , bowtie: mkUndirected
      ["A","B","C","D","E"]
      [t "A" "B", t "A" "C", t "B" "C", t "C" "D", t "C" "E", t "D" "E"]
  }
  where
  t a b = Tuple a b
  mkUndirected nodes edges =
    { nodes
    , edges: foldl (\acc (Tuple a b) ->
        Map.alter (Just <<< Set.insert b <<< fromMaybe Set.empty) a
          (Map.alter (Just <<< Set.insert a <<< fromMaybe Set.empty) b acc)
      ) Map.empty edges
    }
