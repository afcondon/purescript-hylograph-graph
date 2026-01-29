-- | Pathfinding for Honeycomb Puzzle
-- |
-- | BFS to find path between nodes given effective edges
module Honeycomb.Pathfinding where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

import Honeycomb.Types (NodeId(..), EdgeKey, allNodes)

-- | Build adjacency list from edge set
buildAdjacency :: Set EdgeKey -> Map NodeId (Array NodeId)
buildAdjacency edges =
  Array.foldl addEdge emptyAdj (Set.toUnfoldable edges)
  where
  emptyAdj = Map.fromFoldable $ allNodes <#> \n -> Tuple n []

  addEdge adj edge =
    adj
      # Map.update (Just <<< Array.cons edge.b) edge.a
      # Map.update (Just <<< Array.cons edge.a) edge.b

-- | BFS to find shortest path from start to end
-- | Returns Nothing if no path exists, Just path otherwise
findPath :: NodeId -> NodeId -> Set EdgeKey -> Maybe (Array NodeId)
findPath start end edges
  | start == end = Just [start]
  | otherwise = bfs (Set.singleton start) [start] Map.empty
  where
  adj = buildAdjacency edges

  bfs :: Set NodeId -> Array NodeId -> Map NodeId NodeId -> Maybe (Array NodeId)
  bfs visited queue parent
    | Array.null queue = Nothing
    | otherwise =
        case Array.uncons queue of
          Nothing -> Nothing
          Just { head: current, tail: rest } ->
            let
              neighbors = case Map.lookup current adj of
                Nothing -> []
                Just ns -> ns

              unvisited = Array.filter (\n -> not (Set.member n visited)) neighbors

              -- Check if we found the end
              foundEnd = Array.elem end unvisited
            in
              if foundEnd
                then Just $ reconstructPath (Map.insert end current parent) end
                else
                  let
                    newVisited = Array.foldl (flip Set.insert) visited unvisited
                    newQueue = rest <> unvisited
                    newParent = Array.foldl (\p n -> Map.insert n current p) parent unvisited
                  in
                    bfs newVisited newQueue newParent

  reconstructPath :: Map NodeId NodeId -> NodeId -> Array NodeId
  reconstructPath parent node =
    case Map.lookup node parent of
      Nothing -> [node]
      Just p -> reconstructPath parent p `Array.snoc` node

-- | Check if path exists (cheaper than finding full path)
hasPath :: NodeId -> NodeId -> Set EdgeKey -> Boolean
hasPath start end edges = case findPath start end edges of
  Nothing -> false
  Just _ -> true

-- | Get all nodes on the path (as a Set for easy membership check)
pathNodeSet :: Maybe (Array NodeId) -> Set NodeId
pathNodeSet Nothing = Set.empty
pathNodeSet (Just path) = Set.fromFoldable path

-- | Get edges that are on the path
pathEdgeSet :: Maybe (Array NodeId) -> Set EdgeKey
pathEdgeSet Nothing = Set.empty
pathEdgeSet (Just path) =
  Set.fromFoldable $ Array.zipWith mkEdge path (Array.drop 1 path)
  where
  mkEdge a b = if show a < show b then { a, b } else { a: b, b: a }
