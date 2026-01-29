-- | Force Simulation for Honeycomb Puzzle
-- |
-- | Uses SimulationGroup for 6 synchronized graph simulations.
-- | Fully declarative - no Effect Refs in demo code.
module Honeycomb.Simulation where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)

import Hylograph.Kernel.D3.Simulation (SimulationNode, defaultConfig)
import Hylograph.Kernel.D3.SimulationGroup as Group
import Hylograph.Kernel.D3.Setup as Setup
import Hylograph.Kernel.D3.Events (defaultCallbacks)

import Honeycomb.Types (NodeId(..), EdgeKey, allNodes)

-- =============================================================================
-- Types
-- =============================================================================

-- | Node type for Honeycomb simulations
type HoneycombNode = SimulationNode (nodeId :: NodeId)

-- | Node positions keyed by NodeId
type NodePositions = Map NodeId { x :: Number, y :: Number }

-- | Re-export SimulationGroup for use in Component
type HoneycombGroup = Group.SimulationGroup (nodeId :: NodeId) ()

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Create initial nodes for a graph (circle layout centered in viewBox)
createInitialNodes :: Array HoneycombNode
createInitialNodes = Array.mapWithIndex mkNode allNodes
  where
  mkNode idx nodeId =
    let
      angle = (toNumber idx) * 2.0 * pi / 8.0 - pi / 2.0
      radius = 45.0
      cx = 90.0
      cy = 75.0
    in
      { id: idx
      , nodeId: nodeId
      , x: cx + radius * cos angle
      , y: cy + radius * sin angle
      , vx: 0.0
      , vy: 0.0
      , fx: null
      , fy: null
      }

-- | Create a simulation group with 6 simulations
createHoneycombGroup :: Effect HoneycombGroup
createHoneycombGroup = do
  callbacks <- defaultCallbacks
  Group.createGroupWithCallbacks 6 defaultConfig callbacks

-- | Initialize a simulation group with nodes
initializeGroup :: HoneycombGroup -> Effect Unit
initializeGroup group = do
  let nodes = createInitialNodes
  -- Set nodes for all 6 simulations
  for_ (Array.range 0 5) \idx -> Group.setNodesAt idx nodes group

-- =============================================================================
-- Force Setup
-- =============================================================================

-- | Force setup for Honeycomb graphs
-- | ForceX/Y center nodes, ManyBody provides repulsion, Link connects edges
honeycombSetup :: Setup.Setup HoneycombNode
honeycombSetup = Setup.setup "honeycomb"
  [ -- Pull nodes toward center X
    Setup.positionX "centerX"
      # Setup.withX (Setup.static 90.0)
      # Setup.withStrength (Setup.static 0.08)
  , -- Pull nodes toward center Y
    Setup.positionY "centerY"
      # Setup.withY (Setup.static 75.0)
      # Setup.withStrength (Setup.static 0.08)
  , -- Repulsion between nodes
    Setup.manyBody "charge"
      # Setup.withStrength (Setup.static (-120.0))
  , -- Link force for connected nodes
    Setup.link "links"
      # Setup.withDistance (Setup.static 35.0)
      # Setup.withStrength (Setup.static 0.3)
  ]

-- | Apply the force setup to all simulations in the group
applyHoneycombSetup :: HoneycombGroup -> Effect Unit
applyHoneycombSetup = Group.applySetupAll honeycombSetup

-- =============================================================================
-- Edge/Link Management
-- =============================================================================

-- | Update links for a specific simulation based on effective edges
updateLinksAt :: Int -> Set EdgeKey -> HoneycombGroup -> Effect Unit
updateLinksAt idx edges group = do
  let links = edgesToLinks edges
  Group.setLinksAt idx links group

-- | Convert EdgeKeys to link array (source/target are node indices)
edgesToLinks :: Set EdgeKey -> Array { source :: Int, target :: Int }
edgesToLinks edges = Set.toUnfoldable edges <#> edgeToLink
  where
  edgeToLink edge =
    { source: nodeIdToIndex edge.a
    , target: nodeIdToIndex edge.b
    }

-- | Map NodeId to index (0-7)
nodeIdToIndex :: NodeId -> Int
nodeIdToIndex C = 0
nodeIdToIndex G = 1
nodeIdToIndex D = 2
nodeIdToIndex A = 3
nodeIdToIndex E = 4
nodeIdToIndex B = 5
nodeIdToIndex FSharp = 6
nodeIdToIndex CSharp = 7

-- | Map index back to NodeId
indexToNodeId :: Int -> NodeId
indexToNodeId 0 = C
indexToNodeId 1 = G
indexToNodeId 2 = D
indexToNodeId 3 = A
indexToNodeId 4 = E
indexToNodeId 5 = B
indexToNodeId 6 = FSharp
indexToNodeId _ = CSharp

-- =============================================================================
-- Position Extraction
-- =============================================================================

-- | Get current node positions from a specific simulation
getPositionsAt :: Int -> HoneycombGroup -> Effect NodePositions
getPositionsAt idx group = do
  mNodes <- Group.getNodesAt idx group
  case mNodes of
    Nothing -> pure Map.empty
    Just nodes -> pure $ Map.fromFoldable $ nodes <#> \n ->
      Tuple n.nodeId { x: n.x, y: n.y }

-- | Get positions from all 6 simulations
getAllPositions :: HoneycombGroup -> Effect (Array NodePositions)
getAllPositions group = do
  Array.foldM getOne [] (Array.range 0 5)
  where
  getOne acc idx = do
    positions <- getPositionsAt idx group
    pure $ Array.snoc acc positions

-- =============================================================================
-- Math helpers (FFI)
-- =============================================================================

foreign import cos :: Number -> Number
foreign import sin :: Number -> Number

pi :: Number
pi = 3.14159265358979323846
