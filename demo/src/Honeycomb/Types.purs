-- | Types for the Honeycomb Puzzle
-- |
-- | 6 hexagonal graphs, shared edge toggle mechanic, pathfinding puzzle
module Honeycomb.Types where

import Prelude
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (Maybe)

-- | Node identifiers - Circle of Fifths: C G D A E B F# C#
data NodeId = C | G | D | A | E | B | FSharp | CSharp

derive instance eqNodeId :: Eq NodeId
derive instance ordNodeId :: Ord NodeId

instance showNodeId :: Show NodeId where
  show C = "C"
  show G = "G"
  show D = "D"
  show A = "A"
  show E = "E"
  show B = "B"
  show FSharp = "F#"
  show CSharp = "C#"

allNodes :: Array NodeId
allNodes = [C, G, D, A, E, B, FSharp, CSharp]

-- | An edge is an unordered pair of nodes
type EdgeKey = { a :: NodeId, b :: NodeId }

mkEdgeKey :: NodeId -> NodeId -> EdgeKey
mkEdgeKey n1 n2 = if show n1 < show n2
  then { a: n1, b: n2 }
  else { a: n2, b: n1 }

-- | Circle of Fifths path threading
-- | Graph 0: C → G
-- | Graph 1: G → D
-- | Graph 2: D → A
-- | Graph 3: A → E
-- | Graph 4: E → B
-- | Graph 5: B → F#
type PathEndpoints = { start :: NodeId, end :: NodeId }

graphEndpoints :: Int -> PathEndpoints
graphEndpoints 0 = { start: C, end: G }
graphEndpoints 1 = { start: G, end: D }
graphEndpoints 2 = { start: D, end: A }
graphEndpoints 3 = { start: A, end: E }
graphEndpoints 4 = { start: E, end: B }
graphEndpoints 5 = { start: B, end: FSharp }
graphEndpoints _ = { start: C, end: G }

-- | A single graph's base structure
type GraphData =
  { index :: Int
  , baseEdges :: Set EdgeKey
  , nodePositions :: Map NodeId { x :: Number, y :: Number }
  }

-- | Global puzzle state
type PuzzleState =
  { graphs :: Array GraphData
  , globalToggles :: Set EdgeKey  -- XOR with base edges
  , moveCount :: Int
  , scrambleHistory :: Array EdgeKey  -- for solve/reset
  }

-- | Computed per-graph state
type GraphViewState =
  { effectiveEdges :: Set EdgeKey
  , path :: Maybe (Array NodeId)
  , hasPath :: Boolean
  }
