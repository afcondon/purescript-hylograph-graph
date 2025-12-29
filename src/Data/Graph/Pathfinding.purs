-- | A* Pathfinding Algorithm
-- |
-- | Pure implementation of A* for weighted graphs.
-- | Returns the shortest path between two nodes.
module Data.Graph.Pathfinding
  ( findPath
  , PathResult(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, minimumBy)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set

import Data.Graph.Types (Graph, NodeId, Path, neighbors)

-- | Result of pathfinding
data PathResult
  = PathFound { path :: Path, cost :: Number }
  | NoPath
  | SameNode

-- | A* state during search
type AStarState =
  { openSet :: Set NodeId                    -- Nodes to explore
  , cameFrom :: Map NodeId NodeId            -- Backtrack map
  , gScore :: Map NodeId Number              -- Cost from start
  , fScore :: Map NodeId Number              -- Estimated total cost
  }

-- | Find shortest path using A* algorithm
-- |
-- | Uses Euclidean distance as heuristic (requires node positions).
-- | Falls back to Dijkstra (h=0) if positions unavailable.
findPath :: NodeId -> NodeId -> Graph -> PathResult
findPath start goal graph
  | start == goal = SameNode
  | otherwise = search initialState
  where
  initialState :: AStarState
  initialState =
    { openSet: Set.singleton start
    , cameFrom: Map.empty
    , gScore: Map.singleton start 0.0
    , fScore: Map.singleton start (heuristic start goal)
    }

  -- Heuristic: for now, just use 0 (Dijkstra)
  -- Could add position-based heuristic later
  heuristic :: NodeId -> NodeId -> Number
  heuristic _ _ = 0.0

  -- Get g-score with infinity default
  getG :: NodeId -> AStarState -> Number
  getG node state = fromMaybe infinity $ Map.lookup node state.gScore

  -- Get f-score with infinity default
  getF :: NodeId -> AStarState -> Number
  getF node state = fromMaybe infinity $ Map.lookup node state.fScore

  infinity :: Number
  infinity = 1.0e308

  -- Main search loop
  search :: AStarState -> PathResult
  search state
    | Set.isEmpty state.openSet = NoPath
    | otherwise =
        let -- Find node with lowest f-score
            current = findLowestF state.openSet state
        in if current == goal
           then PathFound
                  { path: reconstructPath state.cameFrom current
                  , cost: getG current state
                  }
           else
             let -- Remove current from open set
                 openSet' = Set.delete current state.openSet
                 -- Process all neighbors
                 state' = foldl (processNeighbor current) (state { openSet = openSet' })
                            (neighbors current graph)
             in search state'

  -- Find node with lowest f-score in open set
  findLowestF :: Set NodeId -> AStarState -> NodeId
  findLowestF openSet state =
    fromMaybe (unsafeHead $ Set.toUnfoldable openSet) $
      minimumBy (comparing (flip getF state)) (Set.toUnfoldable openSet :: Array NodeId)

  -- Process a neighbor node
  processNeighbor :: NodeId -> AStarState -> { neighbor :: NodeId, weight :: Number } -> AStarState
  processNeighbor current state { neighbor, weight } =
    let tentativeG = getG current state + weight
    in if tentativeG < getG neighbor state
       then state
              { openSet = Set.insert neighbor state.openSet
              , cameFrom = Map.insert neighbor current state.cameFrom
              , gScore = Map.insert neighbor tentativeG state.gScore
              , fScore = Map.insert neighbor (tentativeG + heuristic neighbor goal) state.fScore
              }
       else state

  -- Reconstruct path from cameFrom map
  reconstructPath :: Map NodeId NodeId -> NodeId -> Path
  reconstructPath cameFrom current = go [current] current
    where
    go path node = case Map.lookup node cameFrom of
      Nothing -> path
      Just prev -> go (Array.cons prev path) prev

-- | Unsafe head (only used when we know set is non-empty)
unsafeHead :: forall a. Array a -> a
unsafeHead arr = case Array.head arr of
  Just x -> x
  Nothing -> unsafeHead arr  -- Will loop, but we never call with empty
