-- | SVG Rendering for Honeycomb Puzzle (using raw Halogen HTML)
module Honeycomb.Render where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Honeycomb.Types (NodeId(..), EdgeKey, mkEdgeKey, allNodes, graphEndpoints)
import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Types
-- =============================================================================

type NodePosition = { x :: Number, y :: Number }

type RenderConfig =
  { width :: Number
  , height :: Number
  , nodeRadius :: Number
  , positions :: Map NodeId NodePosition
  , effectiveEdges :: Set EdgeKey
  , pathNodes :: Set NodeId
  , pathEdges :: Set EdgeKey
  , startNode :: NodeId
  , endNode :: NodeId
  }

-- =============================================================================
-- SVG Namespace helpers
-- =============================================================================

svgNS :: forall r w i. String -> Array (HP.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgNS name attrs children = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name) (unsafeCoerce attrs) children

svgAttr :: forall r i. String -> String -> HP.IProp r i
svgAttr name val = HP.attr (HH.AttrName name) val

-- =============================================================================
-- Default Positions (Circle layout for 8 nodes)
-- =============================================================================

defaultPositions :: Map NodeId NodePosition
defaultPositions = Map.fromFoldable
  [ Tuple C       { x: 90.0,  y: 20.0 }
  , Tuple G       { x: 145.0, y: 40.0 }
  , Tuple D       { x: 160.0, y: 85.0 }
  , Tuple A       { x: 135.0, y: 125.0 }
  , Tuple E       { x: 75.0,  y: 130.0 }
  , Tuple B       { x: 30.0,  y: 100.0 }
  , Tuple FSharp  { x: 20.0,  y: 55.0 }
  , Tuple CSharp  { x: 55.0,  y: 25.0 }
  ]

-- =============================================================================
-- SVG Rendering
-- =============================================================================

-- | Render a graph as SVG
renderGraphSvg :: forall w i. RenderConfig -> (EdgeKey -> i) -> HH.HTML w i
renderGraphSvg config onEdgeClick =
  svgNS "svg"
    [ svgAttr "viewBox" "0 0 180 150"
    , svgAttr "class" "graph-svg-inner"
    ]
    (edges <> nodes)
  where
  edges = renderEdges config onEdgeClick
  nodes = renderNodes config

-- | Render only effective edges (edges that exist in this graph)
renderEdges :: forall w i. RenderConfig -> (EdgeKey -> i) -> Array (HH.HTML w i)
renderEdges config onEdgeClick =
  -- Only render edges that are present (effective)
  Array.filter (\edge -> Set.member edge config.effectiveEdges) allEdgePairs
    <#> \edge -> renderEdge config edge (Set.member edge config.pathEdges) onEdgeClick

-- | All possible edges between nodes
allEdgePairs :: Array EdgeKey
allEdgePairs = do
  n1 <- allNodes
  n2 <- allNodes
  if show n1 < show n2
    then [ mkEdgeKey n1 n2 ]
    else []

-- | Render a single edge (only called for effective/present edges)
renderEdge :: forall w i. RenderConfig -> EdgeKey -> Boolean -> (EdgeKey -> i) -> HH.HTML w i
renderEdge config edge isOnPath onEdgeClick =
  svgNS "g"
    [ svgAttr "class" classes
    , HE.onClick \_ -> onEdgeClick edge
    , svgAttr "style" "cursor: pointer"
    ]
    [ -- Invisible hit area for easier clicking
      svgNS "line"
        [ svgAttr "x1" (show pos1.x)
        , svgAttr "y1" (show pos1.y)
        , svgAttr "x2" (show pos2.x)
        , svgAttr "y2" (show pos2.y)
        , svgAttr "stroke" "transparent"
        , svgAttr "stroke-width" "15"
        ]
        []
    , -- Visible line
      svgNS "line"
        [ svgAttr "x1" (show pos1.x)
        , svgAttr "y1" (show pos1.y)
        , svgAttr "x2" (show pos2.x)
        , svgAttr "y2" (show pos2.y)
        , svgAttr "stroke" strokeColor
        , svgAttr "stroke-width" (show strokeWidth)
        ]
        []
    ]
  where
  pos1 = fromMaybe { x: 0.0, y: 0.0 } $ Map.lookup edge.a config.positions
  pos2 = fromMaybe { x: 0.0, y: 0.0 } $ Map.lookup edge.b config.positions

  classes = "edge active" <> (if isOnPath then " on-path" else "")

  strokeColor
    | isOnPath = "#e0a458"
    | otherwise = "#1d3557"

  strokeWidth
    | isOnPath = 4.0
    | otherwise = 2.0

-- | Render all nodes
renderNodes :: forall w i. RenderConfig -> Array (HH.HTML w i)
renderNodes config = allNodes <#> \nodeId ->
  renderNode config nodeId

-- | Render a single node
renderNode :: forall w i. RenderConfig -> NodeId -> HH.HTML w i
renderNode config nodeId =
  svgNS "g"
    [ svgAttr "class" classes ]
    [ svgNS "circle"
        [ svgAttr "cx" (show pos.x)
        , svgAttr "cy" (show pos.y)
        , svgAttr "r" (show config.nodeRadius)
        , svgAttr "fill" fillColor
        , svgAttr "stroke" "#1d3557"
        , svgAttr "stroke-width" "2"
        ]
        []
    , svgNS "text"
        [ svgAttr "x" (show pos.x)
        , svgAttr "y" (show $ pos.y + 1.0)
        , svgAttr "text-anchor" "middle"
        , svgAttr "dominant-baseline" "middle"
        , svgAttr "fill" "white"
        , svgAttr "font-size" "9"
        , svgAttr "font-weight" "bold"
        , svgAttr "font-family" "Palatino, Georgia, serif"
        ]
        [ HH.text label ]
    ]
  where
  pos = fromMaybe { x: 0.0, y: 0.0 } $ Map.lookup nodeId config.positions

  isStart = nodeId == config.startNode
  isEnd = nodeId == config.endNode
  isOnPath = Set.member nodeId config.pathNodes

  classes = "node"
    <> (if isStart then " start" else "")
    <> (if isEnd then " end" else "")
    <> (if isOnPath && not isStart && not isEnd then " on-path" else "")

  fillColor
    | isStart = "#457b9d"
    | isEnd = "#c44536"
    | isOnPath = "#e0a458"
    | otherwise = "#14213d"

  label = case nodeId of
    FSharp -> "F#"
    CSharp -> "C#"
    _ -> show nodeId

-- =============================================================================
-- Config Builder
-- =============================================================================

mkRenderConfig :: Int -> Set EdgeKey -> Set EdgeKey -> Maybe (Array NodeId) -> RenderConfig
mkRenderConfig graphIndex baseEdges toggles mPath =
  mkRenderConfigWithPositions graphIndex baseEdges toggles mPath Map.empty

-- | Create render config with dynamic positions from simulation
mkRenderConfigWithPositions :: Int -> Set EdgeKey -> Set EdgeKey -> Maybe (Array NodeId) -> Map NodeId NodePosition -> RenderConfig
mkRenderConfigWithPositions graphIndex baseEdges toggles mPath simPositions =
  { width: 180.0
  , height: 150.0
  , nodeRadius: 12.0
  , positions: mergePositions defaultPositions simPositions
  , effectiveEdges: computeEffectiveEdges baseEdges toggles
  , pathNodes: Set.fromFoldable $ fromMaybe [] mPath
  , pathEdges: computePathEdges mPath
  , startNode: endpoints.start
  , endNode: endpoints.end
  }
  where
  endpoints = graphEndpoints graphIndex

-- | Merge simulation positions with defaults (simulation takes precedence)
mergePositions :: Map NodeId NodePosition -> Map NodeId NodePosition -> Map NodeId NodePosition
mergePositions defaults simPos =
  -- If simulation positions are empty, use defaults
  if Map.isEmpty simPos
    then defaults
    else Map.union simPos defaults  -- simPos takes precedence

computeEffectiveEdges :: Set EdgeKey -> Set EdgeKey -> Set EdgeKey
computeEffectiveEdges base toggles =
  Set.union
    (Set.difference base toggles)
    (Set.difference toggles base)

computePathEdges :: Maybe (Array NodeId) -> Set EdgeKey
computePathEdges Nothing = Set.empty
computePathEdges (Just path) =
  Set.fromFoldable $ Array.zipWith mkEdgeKey path (Array.drop 1 path)
