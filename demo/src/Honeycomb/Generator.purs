-- | Random Graph Generation for Honeycomb Puzzle
-- |
-- | Generates 6 graphs with guaranteed paths, then scrambles
module Honeycomb.Generator where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)

import Honeycomb.Types (NodeId(..), EdgeKey, mkEdgeKey, allNodes, graphEndpoints)

-- =============================================================================
-- Graph Generation
-- =============================================================================

type GeneratedGraph =
  { index :: Int
  , baseEdges :: Set EdgeKey
  }

-- | Generate 6 graphs, each with guaranteed path for its endpoints
generateAllGraphs :: Effect (Array GeneratedGraph)
generateAllGraphs = do
  graphs <- Array.foldM generateOne [] (0 .. 5)
  pure graphs
  where
  generateOne acc idx = do
    edges <- generateGraphEdges idx
    pure $ Array.snoc acc { index: idx, baseEdges: edges }

-- | Generate edges for one graph ensuring path exists
generateGraphEdges :: Int -> Effect (Set EdgeKey)
generateGraphEdges idx = do
  let endpoints = graphEndpoints idx

  -- First, create a guaranteed path from start to end
  pathEdges <- generatePath endpoints.start endpoints.end

  -- Then add some random edges for variety
  extraEdges <- generateRandomEdges 5

  pure $ Set.union pathEdges extraEdges

-- | Generate a random path between two nodes through intermediates
generatePath :: NodeId -> NodeId -> Effect (Set EdgeKey)
generatePath start end = do
  -- Pick 1-3 intermediate nodes
  numIntermediates <- randomInt 1 3
  intermediates <- pickRandomNodes numIntermediates [start, end]

  let pathNodes = [start] <> intermediates <> [end]
  let edges = Array.zipWith mkEdgeKey pathNodes (Array.drop 1 pathNodes)

  pure $ Set.fromFoldable edges

-- | Pick n random nodes, excluding some
pickRandomNodes :: Int -> Array NodeId -> Effect (Array NodeId)
pickRandomNodes n exclude = do
  let available = Array.filter (\node -> not (Array.elem node exclude)) allNodes
  pickN n available []
  where
  pickN 0 _ acc = pure acc
  pickN remaining avail acc
    | Array.null avail = pure acc
    | otherwise = do
        idx <- randomInt 0 (Array.length avail - 1)
        case Array.index avail idx of
          Nothing -> pure acc
          Just node -> do
            let newAvail = Array.filter (_ /= node) avail
            pickN (remaining - 1) newAvail (Array.snoc acc node)

-- | Generate some random edges
generateRandomEdges :: Int -> Effect (Set EdgeKey)
generateRandomEdges n = do
  edges <- Array.foldM addRandomEdge [] (0 .. (n - 1))
  pure $ Set.fromFoldable edges
  where
  addRandomEdge acc _ = do
    idx1 <- randomInt 0 (Array.length allNodes - 1)
    idx2 <- randomInt 0 (Array.length allNodes - 1)
    case Tuple (Array.index allNodes idx1) (Array.index allNodes idx2) of
      Tuple (Just n1) (Just n2)
        | n1 /= n2 -> pure $ Array.snoc acc (mkEdgeKey n1 n2)
      _ -> pure acc

-- =============================================================================
-- Scrambling (Rubik's Cube method)
-- =============================================================================

type ScrambleResult =
  { toggles :: Set EdgeKey
  , history :: Array EdgeKey
  }

-- | Scramble by toggling edges that are ON the paths (to break them)
-- | We need to toggle at least one edge per graph to guarantee breaking
scrambleGraphs :: Array GeneratedGraph -> Int -> Effect ScrambleResult
scrambleGraphs graphs _ = do
  -- For each graph, pick 1-2 edges from its base edges to toggle
  -- This guarantees we break at least some paths in each graph
  graphToggles <- Array.foldM pickFromGraph [] graphs

  -- Also add a few random edges for variety/confusion
  randomEdges <- generateScrambleEdges 3

  let allToggles = graphToggles <> randomEdges
  pure
    { toggles: Set.fromFoldable allToggles
    , history: allToggles
    }
  where
  pickFromGraph acc graph = do
    let edges = Set.toUnfoldable graph.baseEdges :: Array EdgeKey
    -- Pick 1-2 edges from this graph's base edges
    numToPick <- randomInt 1 2
    picked <- pickRandomFromArray numToPick edges
    pure $ acc <> picked

-- | Get the edges that form the guaranteed path in a graph
getPathEdges :: GeneratedGraph -> Array EdgeKey
getPathEdges graph = Set.toUnfoldable graph.baseEdges

-- | Pick n random items from an array
pickRandomFromArray :: forall a. Int -> Array a -> Effect (Array a)
pickRandomFromArray n arr = pickN n arr []
  where
  pickN 0 _ acc = pure acc
  pickN remaining avail acc
    | Array.null avail = pure acc
    | otherwise = do
        idx <- randomInt 0 (Array.length avail - 1)
        case Array.index avail idx of
          Nothing -> pure acc
          Just item -> do
            let newAvail = Array.deleteAt idx avail # fromMaybe avail
            pickN (remaining - 1) newAvail (Array.snoc acc item)

-- | Generate random edges to toggle (for variety)
generateScrambleEdges :: Int -> Effect (Array EdgeKey)
generateScrambleEdges n = do
  Array.foldM addEdge [] (0 .. (n - 1))
  where
  addEdge acc _ = do
    idx1 <- randomInt 0 (Array.length allNodes - 1)
    idx2 <- randomInt 0 (Array.length allNodes - 1)
    case Tuple (Array.index allNodes idx1) (Array.index allNodes idx2) of
      Tuple (Just n1) (Just n2)
        | n1 /= n2 -> pure $ Array.snoc acc (mkEdgeKey n1 n2)
      _ -> pure acc

-- =============================================================================
-- Full Puzzle Generation
-- =============================================================================

type Puzzle =
  { graphs :: Array GeneratedGraph
  , initialToggles :: Set EdgeKey
  , scrambleHistory :: Array EdgeKey
  }

-- | Generate a complete puzzle
generatePuzzle :: Int -> Effect Puzzle
generatePuzzle scrambleMoves = do
  graphs <- generateAllGraphs
  scrambleResult <- scrambleGraphs graphs scrambleMoves
  pure
    { graphs
    , initialToggles: scrambleResult.toggles
    , scrambleHistory: scrambleResult.history
    }
