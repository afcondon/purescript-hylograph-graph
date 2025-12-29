module Data.Graph.Algorithms
  ( -- Types
    SimpleGraph
  , TaskNode
  , LayeredNode
  , GraphMetrics
    -- Reachability
  , reachableFrom
    -- Topological sort
  , buildGraph
  , getTopologicalOrder
  , computeLayers
  , addLayers
    -- Transitive reduction
  , transitiveReduction
  , getRemovedEdges
  , getAllEdges
    -- Cycle detection
  , hasCycle
  , findCycle
  , isDAG
    -- Connected components
  , connectedComponents
  , isConnected
    -- Centrality
  , degreeCentrality
  , inDegreeCentrality
  , outDegreeCentrality
    -- Graph metrics
  , graphMetrics
  , density
  , averageDegree
    -- Conversion
  , taskNodesToSimpleGraph
  , simpleGraphToTaskNodes
  ) where

-- | Graph algorithms for DAGs (Directed Acyclic Graphs)
-- | Provides topological sort, transitive reduction, and reachability analysis

import Prelude

import Data.Array (concat, foldl, (\\))
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Graph (Graph, fromMap, topologicalSort)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- =============================================================================
-- || Simple Graph Representation (Adjacency List)
-- =============================================================================

-- | Simple graph as adjacency list (node -> set of targets)
type SimpleGraph node =
  { nodes :: Array node
  , edges :: Map node (Set node)
  }

-- =============================================================================
-- || Reachability Analysis
-- =============================================================================

-- | Compute all nodes reachable from a given node via BFS
-- | Uses tail recursion for efficiency
reachableFrom :: forall node. Ord node => node -> SimpleGraph node -> Set node
reachableFrom start graph =
  go Set.empty (Set.singleton start)
  where
    go :: Set node -> Set node -> Set node
    go visited frontier
      | Set.isEmpty frontier = visited
      | otherwise =
          let
            -- Get immediate neighbors of all frontier nodes
            neighbors = Set.unions $ Array.catMaybes $
              Set.toUnfoldable frontier <#> \n ->
                Map.lookup n graph.edges

            -- Only visit new nodes
            newNodes = Set.difference neighbors visited
          in
            go (Set.union visited frontier) newNodes

-- =============================================================================
-- || Topological Sort with Layers
-- =============================================================================

-- | A node with dependencies
type TaskNode node =
  { id :: node
  , depends :: Array node
  }

-- | A node with computed layer information
type LayeredNode node =
  { id :: node
  , layer :: Int
  , depends :: Array node
  }

-- | Build a Data.Graph from task nodes
-- | Graph k v = Graph (Map k (Tuple v (List k)))
buildGraph :: forall node. Ord node => Array (TaskNode node) -> Graph node (TaskNode node)
buildGraph tasks =
  let
    graphMap :: Map node (Tuple (TaskNode node) (List node))
    graphMap = foldl addTask Map.empty tasks

    addTask :: Map node (Tuple (TaskNode node) (List node)) -> TaskNode node -> Map node (Tuple (TaskNode node) (List node))
    addTask acc task =
      Map.insert task.id (Tuple task (List.fromFoldable task.depends)) acc
  in
    fromMap graphMap

-- | Get topological order using Data.Graph.topologicalSort
getTopologicalOrder :: forall node. Ord node => Array (TaskNode node) -> List node
getTopologicalOrder tasks =
  let graph = buildGraph tasks
  in topologicalSort graph

-- | Compute layers from topological sort
-- | Layer 0: nodes with no dependencies
-- | Layer N: nodes whose dependencies are all in layers < N
computeLayers :: forall node. Ord node => Array (TaskNode node) -> Map node Int
computeLayers tasks =
  let
    -- Reverse to get dependencies first
    sortedIds = List.reverse $ getTopologicalOrder tasks
    taskMap = Map.fromFoldable $ tasks <#> \t -> Tuple t.id t

    processTask :: Map node Int -> node -> Map node Int
    processTask accLayers taskId =
      case Map.lookup taskId taskMap of
        Nothing -> accLayers
        Just task ->
          let
            layer =
              if Array.null task.depends
                then 0
                else
                  -- All dependencies were processed earlier (topological order!)
                  let depLayers = task.depends <#> \depId ->
                        fromMaybe 0 $ Map.lookup depId accLayers
                  in case maximum depLayers of
                       Just maxLayer -> maxLayer + 1
                       Nothing -> 0
          in Map.insert taskId layer accLayers
  in
    foldl processTask Map.empty (List.toUnfoldable sortedIds :: Array node)

-- | Add layer information to task nodes
addLayers :: forall node. Ord node => Array (TaskNode node) -> Array (LayeredNode node)
addLayers tasks =
  let layers = computeLayers tasks
  in tasks <#> \t ->
       { id: t.id
       , layer: fromMaybe 0 $ Map.lookup t.id layers
       , depends: t.depends
       }

-- =============================================================================
-- || Transitive Reduction
-- =============================================================================

-- | Remove transitive edges from a graph
-- | An edge A → C is transitive if there exists a path A → B → C
-- | Returns a new graph with only the essential (non-transitive) edges
transitiveReduction :: forall node. Ord node => SimpleGraph node -> SimpleGraph node
transitiveReduction graph =
  { nodes: graph.nodes
  , edges: Map.fromFoldable $ map reduceEdge $ (Map.toUnfoldable graph.edges :: Array (Tuple node (Set node)))
  }
  where
    reduceEdge :: Tuple node (Set node) -> Tuple node (Set node)
    reduceEdge (Tuple source targets) =
      Tuple source (Set.filter (not <<< isTransitive source) targets)

    isTransitive :: node -> node -> Boolean
    isTransitive source target =
      let
        -- Get all intermediate nodes (direct targets except this one)
        intermediates = Set.delete target $ fromMaybe Set.empty $ Map.lookup source graph.edges

        -- Check if target is reachable through any intermediate
        reachableThroughIntermediate =
          Array.any (\intermediate ->
            Set.member target $ reachableFrom intermediate graph
          ) (Set.toUnfoldable intermediates)
      in
        reachableThroughIntermediate

-- | Get edges that were removed by transitive reduction
getRemovedEdges :: forall node. Ord node => SimpleGraph node -> SimpleGraph node -> Array (Tuple node node)
getRemovedEdges original reduced =
  let
    originalEdges = getAllEdges original
    reducedEdges = getAllEdges reduced
  in
    originalEdges \\ reducedEdges

-- | Get all edges from a graph as an array of tuples
getAllEdges :: forall node. Ord node => SimpleGraph node -> Array (Tuple node node)
getAllEdges graph =
  concat $ graph.nodes <#> \source ->
    case Map.lookup source graph.edges of
      Nothing -> []
      Just targets -> Set.toUnfoldable targets <#> \target ->
        Tuple source target

-- =============================================================================
-- || Conversion Utilities
-- =============================================================================

-- | Convert TaskNode array to SimpleGraph
taskNodesToSimpleGraph :: forall node. Ord node => Array (TaskNode node) -> SimpleGraph node
taskNodesToSimpleGraph tasks =
  { nodes: tasks <#> _.id
  , edges: Map.fromFoldable $ tasks <#> \task ->
      Tuple task.id (Set.fromFoldable task.depends)
  }

-- | Convert SimpleGraph to TaskNode array
simpleGraphToTaskNodes :: forall node. Ord node => SimpleGraph node -> Array (TaskNode node)
simpleGraphToTaskNodes graph =
  graph.nodes <#> \node ->
    { id: node
    , depends: case Map.lookup node graph.edges of
        Nothing -> []
        Just targets -> Set.toUnfoldable targets
    }

-- =============================================================================
-- || Cycle Detection
-- =============================================================================

-- | Check if a directed graph has a cycle using DFS with coloring
-- |
-- | Uses three colors:
-- | - White (unvisited)
-- | - Gray (currently in DFS stack)
-- | - Black (completely processed)
-- |
-- | A back edge to a gray node indicates a cycle.
hasCycle :: forall node. Ord node => SimpleGraph node -> Boolean
hasCycle graph = case findCycle graph of
  Nothing -> false
  Just _ -> true

-- | Find a cycle in the graph, if one exists
-- |
-- | Returns the nodes forming the cycle (in order).
findCycle :: forall node. Ord node => SimpleGraph node -> Maybe (Array node)
findCycle graph =
  let
    -- DFS state: white = unvisited, gray = in stack, black = done
    initialState = { white: Set.fromFoldable graph.nodes, gray: Set.empty, black: Set.empty, path: [], cycle: Nothing }

    result = foldl (\state node ->
      case state.cycle of
        Just _ -> state  -- Already found a cycle
        Nothing ->
          if Set.member node state.white
          then dfs state node
          else state
    ) initialState graph.nodes
  in result.cycle
  where
  dfs state node
    | Set.member node state.gray =
        -- Found a back edge - we have a cycle!
        -- Extract the cycle from path
        let cycleStart = Array.findIndex (_ == node) state.path
        in case cycleStart of
             Just idx -> state { cycle = Just (Array.drop idx state.path <> [node]) }
             Nothing -> state { cycle = Just [node] }
    | Set.member node state.black = state  -- Already fully processed
    | otherwise =
        let
          -- Move from white to gray
          state' = state
            { white = Set.delete node state.white
            , gray = Set.insert node state.gray
            , path = Array.snoc state.path node
            }
          -- Visit all neighbors
          neighbors = case Map.lookup node graph.edges of
            Nothing -> []
            Just targets -> Set.toUnfoldable targets
          state'' = foldl dfs state' neighbors
        in
          case state''.cycle of
            Just _ -> state''  -- Cycle found, propagate up
            Nothing ->
              -- Move from gray to black, remove from path
              state''
                { gray = Set.delete node state''.gray
                , black = Set.insert node state''.black
                , path = fromMaybe [] (Array.init state''.path)
                }

-- | Check if graph is a DAG (Directed Acyclic Graph)
isDAG :: forall node. Ord node => SimpleGraph node -> Boolean
isDAG = not <<< hasCycle

-- =============================================================================
-- || Connected Components
-- =============================================================================

-- | Find all connected components in an undirected graph
-- |
-- | Note: Treats the graph as undirected (ignores edge direction).
-- | Returns an array of sets, where each set is a connected component.
connectedComponents :: forall node. Ord node => SimpleGraph node -> Array (Set node)
connectedComponents graph =
  let
    -- Build undirected adjacency
    undirectedEdges = buildUndirectedEdges graph

    result = foldl (\{ visited, components } node ->
      if Set.member node visited
      then { visited, components }
      else
        let component = bfsComponent node undirectedEdges visited
        in { visited: Set.union visited component
           , components: Array.snoc components component
           }
    ) { visited: Set.empty, components: [] } graph.nodes
  in result.components
  where
  -- Build symmetric adjacency for undirected traversal
  buildUndirectedEdges :: SimpleGraph node -> Map node (Set node)
  buildUndirectedEdges g =
    foldl (\acc (Tuple src targets) ->
      let
        -- Add forward edges
        acc' = Map.alter (Just <<< Set.union targets <<< fromMaybe Set.empty) src acc
        -- Add reverse edges
        acc'' = foldl (\a tgt ->
          Map.alter (Just <<< Set.insert src <<< fromMaybe Set.empty) tgt a
        ) acc' (Set.toUnfoldable targets :: Array node)
      in acc''
    ) Map.empty (Map.toUnfoldable g.edges :: Array (Tuple node (Set node)))

  -- BFS to find all nodes in a component
  bfsComponent :: node -> Map node (Set node) -> Set node -> Set node
  bfsComponent start edges visited = go (Set.singleton start) (Set.singleton start)
    where
    go component frontier
      | Set.isEmpty frontier = component
      | otherwise =
          let
            neighbors = Set.unions $ Array.catMaybes $
              (Set.toUnfoldable frontier :: Array node) <#> \n -> Map.lookup n edges
            newNodes = Set.difference neighbors component
            newNodes' = Set.difference newNodes visited
          in go (Set.union component newNodes') newNodes'

-- | Check if the graph is connected (as undirected)
isConnected :: forall node. Ord node => SimpleGraph node -> Boolean
isConnected graph =
  case Array.head graph.nodes of
    Nothing -> true  -- Empty graph is connected
    Just _ ->
      let components = connectedComponents graph
      in Array.length components <= 1

-- =============================================================================
-- || Centrality Measures
-- =============================================================================

-- | Compute degree centrality for all nodes
-- |
-- | Degree centrality = (in-degree + out-degree) / (n - 1)
-- | where n is the number of nodes
degreeCentrality :: forall node. Ord node => SimpleGraph node -> Map node Number
degreeCentrality graph =
  let
    n = Array.length graph.nodes
    normalizer = if n <= 1 then 1.0 else toNumber (n - 1)
    inDegrees = inDegreeCentrality graph
    outDegrees = outDegreeCentrality graph
  in
    Map.fromFoldable $ graph.nodes <#> \node ->
      let
        inDeg = fromMaybe 0.0 $ Map.lookup node inDegrees
        outDeg = fromMaybe 0.0 $ Map.lookup node outDegrees
      in Tuple node ((inDeg + outDeg) * toNumber (n - 1) / normalizer)

-- | Compute in-degree centrality
-- |
-- | In-degree centrality = in-degree / (n - 1)
inDegreeCentrality :: forall node. Ord node => SimpleGraph node -> Map node Number
inDegreeCentrality graph =
  let
    n = Array.length graph.nodes
    normalizer = if n <= 1 then 1.0 else toNumber (n - 1)

    -- Count incoming edges for each node
    inDegrees = foldl (\acc (Tuple _ targets) ->
      foldl (\a tgt ->
        Map.alter (Just <<< (_ + 1) <<< fromMaybe 0) tgt a
      ) acc (Set.toUnfoldable targets :: Array node)
    ) (Map.fromFoldable $ graph.nodes <#> \n' -> Tuple n' 0)
      (Map.toUnfoldable graph.edges :: Array (Tuple node (Set node)))
  in
    map (\deg -> toNumber deg / normalizer) inDegrees

-- | Compute out-degree centrality
-- |
-- | Out-degree centrality = out-degree / (n - 1)
outDegreeCentrality :: forall node. Ord node => SimpleGraph node -> Map node Number
outDegreeCentrality graph =
  let
    n = Array.length graph.nodes
    normalizer = if n <= 1 then 1.0 else toNumber (n - 1)
  in
    Map.fromFoldable $ graph.nodes <#> \node ->
      let outDeg = case Map.lookup node graph.edges of
            Nothing -> 0
            Just targets -> Set.size targets
      in Tuple node (toNumber outDeg / normalizer)

-- =============================================================================
-- || Graph Metrics
-- =============================================================================

-- | Comprehensive metrics about a graph
type GraphMetrics =
  { nodeCount :: Int
  , edgeCount :: Int
  , density :: Number
  , averageDegree :: Number
  , isConnected :: Boolean
  , isDAG :: Boolean
  , componentCount :: Int
  }

-- | Compute all basic metrics for a graph
graphMetrics :: forall node. Ord node => SimpleGraph node -> GraphMetrics
graphMetrics graph =
  let
    nodeCount = Array.length graph.nodes
    edgeCount = Array.length (getAllEdges graph)
    components = connectedComponents graph
  in
    { nodeCount
    , edgeCount
    , density: density graph
    , averageDegree: averageDegree graph
    , isConnected: Array.length components <= 1
    , isDAG: isDAG graph
    , componentCount: Array.length components
    }

-- | Graph density: ratio of actual edges to possible edges
-- |
-- | For directed graphs: |E| / (|V| * (|V| - 1))
-- | Returns 0 for graphs with fewer than 2 nodes
density :: forall node. Ord node => SimpleGraph node -> Number
density graph =
  let
    n = Array.length graph.nodes
    e = Array.length (getAllEdges graph)
    maxEdges = n * (n - 1)
  in
    if maxEdges == 0
    then 0.0
    else toNumber e / toNumber maxEdges

-- | Average degree (in + out) per node
averageDegree :: forall node. Ord node => SimpleGraph node -> Number
averageDegree graph =
  let
    n = Array.length graph.nodes
    e = Array.length (getAllEdges graph)
  in
    if n == 0
    then 0.0
    else toNumber (2 * e) / toNumber n  -- Each edge contributes to 2 degrees

-- | Helper to convert Int to Number
toNumber :: Int -> Number
toNumber = Int.toNumber
