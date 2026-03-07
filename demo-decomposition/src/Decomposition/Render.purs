-- | HATS-based rendering for decomposition-annotated graphs and chimera views.
-- |
-- | Two outputs per graph:
-- | 1. Annotated graph: hand-positioned nodes colored by block, APs as diamonds, bridges dashed
-- | 2. Chimera view: each block excerpted and rendered in its natural visualization form
module Decomposition.Render where

import Prelude

import Data.Array (foldl, length, mapWithIndex, sortBy, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pi, cos, sin)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Hylograph.HATS (Tree, elem, staticStr, staticNum, empty)
import Hylograph.Internal.Element.Types (ElementType(..))

import Data.Graph.Algorithms (SimpleGraph)
import Data.Graph.Decomposition as Dec

import Decomposition.Graphs (Pos)

-- Re-export Tree for Component
type HatsTree = Tree

-- =============================================================================
-- Block palette — Tableau 10
-- =============================================================================

type PaletteEntry = { fill :: String, stroke :: String, light :: String }

blockPalette :: Array PaletteEntry
blockPalette =
  [ { fill: "#4e79a7", stroke: "#3a5d80", light: "#c5d5e8" }  -- steel blue
  , { fill: "#e15759", stroke: "#b8393b", light: "#f2c0c1" }  -- coral
  , { fill: "#59a14f", stroke: "#3f7a38", light: "#c4dfc0" }  -- green
  , { fill: "#f28e2b", stroke: "#c47020", light: "#fbd5ab" }  -- orange
  , { fill: "#b07aa1", stroke: "#8c5e80", light: "#dcc8d6" }  -- mauve
  , { fill: "#edc948", stroke: "#c4a438", light: "#f6e9b4" }  -- gold
  , { fill: "#76b7b2", stroke: "#5a908c", light: "#c8dedd" }  -- teal
  , { fill: "#ff9da7", stroke: "#d47a83", light: "#ffd9dd" }  -- pink
  , { fill: "#9c755f", stroke: "#7a5b49", light: "#d6c4b8" }  -- brown
  , { fill: "#bab0ac", stroke: "#958a85", light: "#ddd8d6" }  -- warm grey
  ]

paletteFor :: Int -> PaletteEntry
paletteFor i = fromMaybe { fill: "#888", stroke: "#666", light: "#ddd" } $ blockPalette !! (i `mod` length blockPalette)

-- =============================================================================
-- Decomposition analysis
-- =============================================================================

type BlockInfo =
  { index :: Int
  , nodes :: Set String
  , edges :: Set (Tuple String String)
  , isBridge :: Boolean  -- single-edge component
  }

type DecompInfo =
  { nodeBlock :: Map String Int
  , edgeBlock :: Map (Tuple String String) Int
  , aps :: Set String
  , bridgeSet :: Set (Tuple String String)
  , blocks :: Array BlockInfo
  , metrics :: Dec.DecompositionMetrics
  , bipartite :: Map Int (Either (Array String) { partA :: Set String, partB :: Set String })
  }

analyzeGraph :: SimpleGraph String -> DecompInfo
analyzeGraph graph =
  let
    comps = Dec.biconnectedComponents graph
    aps = Dec.articulationPoints graph
    br = Dec.bridges graph
    metrics = Dec.decompositionMetrics graph

    edgeBlock = foldl (\acc (Tuple blockIdx edgeSet) ->
      foldl (\m edge -> Map.insert edge blockIdx m) acc
        (Set.toUnfoldable edgeSet :: Array (Tuple String String))
    ) Map.empty (mapWithIndex Tuple comps)

    nodeBlock = foldl (\acc (Tuple blockIdx edgeSet) ->
      foldl (\m (Tuple a b) ->
        let m' = Map.insertWith (\old _ -> old) a blockIdx m
        in Map.insertWith (\old _ -> old) b blockIdx m'
      ) acc (Set.toUnfoldable edgeSet :: Array (Tuple String String))
    ) Map.empty (mapWithIndex Tuple comps)

    bridgeSet = foldl (\s (Tuple a b) ->
      Set.insert (Tuple a b) (Set.insert (Tuple b a) s)
    ) Set.empty br

    blocks = mapWithIndex (\i edgeSet ->
      let nodes = foldl (\acc (Tuple a b) -> Set.insert a (Set.insert b acc)) Set.empty
                    (Set.toUnfoldable edgeSet :: Array (Tuple String String))
      in { index: i, nodes, edges: edgeSet, isBridge: Set.size edgeSet == 1 }
    ) comps

    -- Test bipartiteness per block
    bipartite = foldl (\m block ->
      if block.isBridge then m
      else
        let subgraph = blockToGraph block
        in Map.insert block.index (Dec.detectBipartite subgraph) m
    ) Map.empty blocks
  in
    { nodeBlock, edgeBlock, aps, bridgeSet, blocks, metrics, bipartite }

blockToGraph :: BlockInfo -> SimpleGraph String
blockToGraph block =
  let nodes = Set.toUnfoldable block.nodes :: Array String
      edges = foldl (\acc (Tuple a b) ->
        Map.alter (Just <<< Set.insert b <<< fromMaybe Set.empty) a
          (Map.alter (Just <<< Set.insert a <<< fromMaybe Set.empty) b acc)
      ) Map.empty (Set.toUnfoldable block.edges :: Array (Tuple String String))
  in { nodes, edges }

-- =============================================================================
-- Block shape classification
-- =============================================================================

data BlockShape
  = ShapeDense       -- density > 0.5 → matrix
  | ShapeBipartite   -- bipartite → two-column
  | ShapeTree        -- tree (bridges only) → won't happen for blocks > 1 edge
  | ShapeCycle       -- single cycle → arc diagram
  | ShapeSparse      -- low density → node-link

classifyBlock :: DecompInfo -> BlockInfo -> BlockShape
classifyBlock info block
  | block.isBridge = ShapeTree
  | otherwise =
    let
      n = Set.size block.nodes
      e = Set.size block.edges
      maxEdges = n * (n - 1) / 2
      density = if maxEdges > 0 then toNumber e / toNumber maxEdges else 0.0
      isBip = case Map.lookup block.index info.bipartite of
        Just (Right _) -> true
        _ -> false
    in
      if density > 0.5 then ShapeDense
      else if isBip && n > 3 then ShapeBipartite
      else if e == n then ShapeCycle
      else ShapeSparse

shapeLabel :: BlockShape -> String
shapeLabel ShapeDense = "matrix"
shapeLabel ShapeBipartite = "bipartite"
shapeLabel ShapeTree = "bridge"
shapeLabel ShapeCycle = "cycle"
shapeLabel ShapeSparse = "node-link"

-- =============================================================================
-- HATS Annotated Graph (hand-positioned)
-- =============================================================================

annotatedGraphTree :: SimpleGraph String -> Map String Pos -> DecompInfo -> Tree
annotatedGraphTree graph positions info =
  elem SVG
    [ staticStr "viewBox" "0 0 900 320"
    , staticStr "preserveAspectRatio" "xMidYMid meet"
    ]
    (edgeElements <> nodeElements)
  where
  edgeElements = Array.concatMap (\(Tuple node targets) ->
    Array.mapMaybe (\target ->
      if node < target
      then Just (hatsEdge positions info node target)
      else Nothing
    ) (Set.toUnfoldable targets :: Array String)
  ) (Map.toUnfoldable graph.edges :: Array (Tuple String (Set String)))

  regularNodes = Array.filter (\n -> not (Set.member n info.aps)) graph.nodes
  apNodes = Array.filter (\n -> Set.member n info.aps) graph.nodes
  nodeElements = map (hatsNode positions info false) regularNodes
               <> map (hatsNode positions info true) apNodes

hatsEdge :: Map String Pos -> DecompInfo -> String -> String -> Tree
hatsEdge positions info a b =
  let
    posA = fromMaybe { x: 0.0, y: 0.0 } $ Map.lookup a positions
    posB = fromMaybe { x: 0.0, y: 0.0 } $ Map.lookup b positions
    isBridge = Set.member (Tuple a b) info.bridgeSet
    blockIdx = Map.lookup (Tuple a b) info.edgeBlock
                <|> Map.lookup (Tuple b a) info.edgeBlock
    color = case blockIdx of
      Just i -> (paletteFor i).stroke
      Nothing -> "#888"
  in
    elem Line
      [ staticNum "x1" posA.x
      , staticNum "y1" posA.y
      , staticNum "x2" posB.x
      , staticNum "y2" posB.y
      , staticStr "stroke" (if isBridge then "#333" else color)
      , staticStr "stroke-width" (if isBridge then "3" else "2")
      , staticStr "stroke-dasharray" (if isBridge then "6,4" else "")
      , staticStr "stroke-opacity" "0.7"
      ]
      []

hatsNode :: Map String Pos -> DecompInfo -> Boolean -> String -> Tree
hatsNode positions info isAP node =
  let
    pos = fromMaybe { x: 0.0, y: 0.0 } $ Map.lookup node positions
    blockIdx = fromMaybe 0 $ Map.lookup node info.nodeBlock
    pal = paletteFor blockIdx
    r = if isAP then 14.0 else 10.0
  in
    elem Group []
      [ if isAP
        then
          elem Polygon
            [ staticStr "points" (diamondPoints pos.x pos.y (r * 1.2))
            , staticStr "fill" "#fff"
            , staticStr "stroke" "#333"
            , staticStr "stroke-width" "2.5"
            ]
            []
        else
          elem Circle
            [ staticNum "cx" pos.x
            , staticNum "cy" pos.y
            , staticNum "r" r
            , staticStr "fill" pal.fill
            , staticStr "stroke" pal.stroke
            , staticStr "stroke-width" "2"
            ]
            []
      , elem Text
          [ staticNum "x" pos.x
          , staticNum "y" (pos.y + 1.0)
          , staticStr "text-anchor" "middle"
          , staticStr "dominant-baseline" "middle"
          , staticStr "fill" (if isAP then "#333" else "#fff")
          , staticStr "font-size" "10"
          , staticStr "font-weight" "600"
          , staticStr "font-family" "system-ui, -apple-system, sans-serif"
          ]
          []
      ]

diamondPoints :: Number -> Number -> Number -> String
diamondPoints cx cy r =
  show cx <> "," <> show (cy - r) <> " "
  <> show (cx + r) <> "," <> show cy <> " "
  <> show cx <> "," <> show (cy + r) <> " "
  <> show (cx - r) <> "," <> show cy

-- =============================================================================
-- Full Adjacency Matrix
-- =============================================================================

-- | Alphabetically ordered matrix (before decomposition)
rawMatrixTree :: SimpleGraph String -> DecompInfo -> Tree
rawMatrixTree graph info =
  matrixTreeWith (sortBy compare graph.nodes) graph info false

-- | Block-ordered matrix (after decomposition)
blockMatrixTree :: SimpleGraph String -> DecompInfo -> Tree
blockMatrixTree graph info =
  matrixTreeWith (blockOrderedNodes graph info) graph info true

blockOrderedNodes :: SimpleGraph String -> DecompInfo -> Array String
blockOrderedNodes graph info =
  sortBy (\a b ->
    let ba = fromMaybe 999 $ Map.lookup a info.nodeBlock
        bb = fromMaybe 999 $ Map.lookup b info.nodeBlock
    in case compare ba bb of
      EQ -> compare a b
      other -> other
  ) graph.nodes

matrixTreeWith :: Array String -> SimpleGraph String -> DecompInfo -> Boolean -> Tree
matrixTreeWith orderedNodes graph info showBoundaries =
  let

    n = length orderedNodes
    -- Scale cell size to fit in viewBox
    labelMargin = 30.0
    maxGrid = 280.0
    cellSize = min 16.0 (maxGrid / toNumber n)
    gridSize = cellSize * toNumber n
    totalSize = gridSize + labelMargin + 10.0

    -- Block boundary lines (only when block-ordered)
    blockBounds = if showBoundaries then findBlockBoundaries orderedNodes info.nodeBlock else []
    separators = map (\pos ->
      let p = labelMargin + toNumber pos * cellSize
      in elem Group []
        [ elem Line
            [ staticNum "x1" p, staticNum "y1" labelMargin
            , staticNum "x2" p, staticNum "y2" (labelMargin + gridSize)
            , staticStr "stroke" "#333"
            , staticStr "stroke-width" "1.5"
            , staticStr "stroke-opacity" "0.3"
            ]
            []
        , elem Line
            [ staticNum "x1" labelMargin, staticNum "y1" p
            , staticNum "x2" (labelMargin + gridSize), staticNum "y2" p
            , staticStr "stroke" "#333"
            , staticStr "stroke-width" "1.5"
            , staticStr "stroke-opacity" "0.3"
            ]
            []
        ]
    ) blockBounds

    -- Grid cells
    cells = Array.concatMap (\(Tuple ri rowNode) ->
      let rowBlock = fromMaybe 0 $ Map.lookup rowNode info.nodeBlock
          pal = paletteFor rowBlock
      in Array.concatMap (\(Tuple ci colNode) ->
        let
          colBlock = fromMaybe 0 $ Map.lookup colNode info.nodeBlock
          hasEdge = case Map.lookup rowNode graph.edges of
            Just targets -> Set.member colNode targets
            Nothing -> false
          isDiag = rowNode == colNode
          -- Color: same block = block color, cross-block edge = grey
          fillColor
            | isDiag = pal.light
            | hasEdge && rowBlock == colBlock = pal.fill
            | hasEdge = "#999"
            | otherwise = ""
        in
          if fillColor == "" then []
          else
            [ elem Rect
                [ staticNum "x" (labelMargin + toNumber ci * cellSize)
                , staticNum "y" (labelMargin + toNumber ri * cellSize)
                , staticNum "width" (cellSize - 0.5)
                , staticNum "height" (cellSize - 0.5)
                , staticStr "fill" fillColor
                , staticStr "opacity" (if isDiag then "0.3" else if hasEdge && rowBlock == colBlock then "0.85" else "0.4")
                , staticStr "rx" "1"
                ]
                []
            ]
      ) (mapWithIndex Tuple orderedNodes)
    ) (mapWithIndex Tuple orderedNodes)

    -- Row and column labels
    labels = Array.concatMap (\(Tuple i node) ->
      let blockIdx = fromMaybe 0 $ Map.lookup node info.nodeBlock
          pal = paletteFor blockIdx
          isAP = Set.member node info.aps
          fontWeight = if isAP then "700" else "400"
      in
        [ elem Text
            [ staticNum "x" (labelMargin - 3.0)
            , staticNum "y" (labelMargin + toNumber i * cellSize + cellSize / 2.0 + 1.0)
            , staticStr "text-anchor" "end"
            , staticStr "font-size" (if n > 16 then "6" else "8")
            , staticStr "font-weight" fontWeight
            , staticStr "fill" pal.fill
            , staticStr "textContent" node
            ]
            []
        , elem Text
            [ staticNum "x" (labelMargin + toNumber i * cellSize + cellSize / 2.0)
            , staticNum "y" (labelMargin - 3.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" (if n > 16 then "6" else "8")
            , staticStr "font-weight" fontWeight
            , staticStr "fill" pal.fill
            , staticStr "textContent" node
            ]
            []
        ]
    ) (mapWithIndex Tuple orderedNodes)

    -- Grid background
    gridBg =
      [ elem Rect
          [ staticNum "x" labelMargin
          , staticNum "y" labelMargin
          , staticNum "width" gridSize
          , staticNum "height" gridSize
          , staticStr "fill" "#f8f8f8"
          , staticStr "stroke" "#e0e0e0"
          , staticStr "stroke-width" "1"
          ]
          []
      ]
  in
    elem SVG
      [ staticStr "viewBox" ("0 0 " <> show totalSize <> " " <> show totalSize)
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      ]
      (gridBg <> cells <> separators <> labels)

-- | Find positions where block changes (for separator lines)
findBlockBoundaries :: Array String -> Map String Int -> Array Int
findBlockBoundaries nodes nodeBlock =
  let
    indexed = mapWithIndex Tuple nodes
  in
    Array.mapMaybe (\(Tuple i node) ->
      if i == 0 then Nothing
      else
        let prevBlock = fromMaybe (-1) $ do
              prev <- nodes !! (i - 1)
              Map.lookup prev nodeBlock
            curBlock = fromMaybe (-1) $ Map.lookup node nodeBlock
        in if prevBlock /= curBlock then Just i else Nothing
    ) indexed

-- =============================================================================
-- Chimera View — each block in its natural form
-- =============================================================================

chimeraTree :: DecompInfo -> Tree
chimeraTree info =
  let
    -- Only render non-bridge blocks (bridges are just connectors)
    realBlocks = Array.filter (\b -> not b.isBridge) info.blocks
    -- Sort by size descending
    sorted = sortBy (\a b -> compare (Set.size b.nodes) (Set.size a.nodes)) realBlocks
    n = length sorted
    -- Lay out blocks horizontally
    blockWidth = 180.0
    totalWidth = toNumber n * blockWidth
    startX = (900.0 - totalWidth) / 2.0
    blockTrees = mapWithIndex (\i block ->
      let
        x = startX + toNumber i * blockWidth
        shape = classifyBlock info block
        pal = paletteFor block.index
      in
        elem Group
          [ staticStr "transform" ("translate(" <> show (x + blockWidth / 2.0) <> ",140)")
          ]
          [ -- Shape label above
            elem Text
              [ staticNum "x" 0.0
              , staticNum "y" (-120.0)
              , staticStr "text-anchor" "middle"
              , staticStr "font-size" "11"
              , staticStr "font-weight" "600"
              , staticStr "fill" pal.fill
              , staticStr "textContent" (shapeLabel shape)
              ]
              []
          -- The visualization
          , renderBlock info block shape pal
          -- Node count below
          , elem Text
              [ staticNum "x" 0.0
              , staticNum "y" 125.0
              , staticStr "text-anchor" "middle"
              , staticStr "font-size" "10"
              , staticStr "fill" "#999"
              , staticStr "textContent" (show (Set.size block.nodes) <> " nodes")
              ]
              []
          ]
    ) sorted
    -- Connection lines between blocks (at cut vertices)
    connectors = if n > 1
      then [ elem Line
               [ staticNum "x1" (startX + blockWidth / 2.0)
               , staticNum "y1" 140.0
               , staticNum "x2" (startX + toNumber (n - 1) * blockWidth + blockWidth / 2.0)
               , staticNum "y2" 140.0
               , staticStr "stroke" "#ccc"
               , staticStr "stroke-width" "1"
               , staticStr "stroke-dasharray" "4,4"
               ]
               []
           ]
      else []
  in
    elem SVG
      [ staticStr "viewBox" "0 0 900 280"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      ]
      (connectors <> blockTrees)

renderBlock :: DecompInfo -> BlockInfo -> BlockShape -> PaletteEntry -> Tree
renderBlock info block shape pal = case shape of
  ShapeDense -> renderMatrix block pal
  ShapeBipartite -> renderBipartite info block pal
  ShapeCycle -> renderArc block pal
  ShapeSparse -> renderNodeLink block pal
  ShapeTree -> empty  -- bridges not rendered

-- | Dense block → adjacency matrix
renderMatrix :: BlockInfo -> PaletteEntry -> Tree
renderMatrix block pal =
  let
    nodes = sortBy compare (Set.toUnfoldable block.nodes :: Array String)
    n = length nodes
    cellSize = min 20.0 (90.0 / toNumber n)
    gridSize = cellSize * toNumber n
    offset = -gridSize / 2.0
    cells = Array.concatMap (\(Tuple ri row) ->
      Array.mapMaybe (\(Tuple ci col) ->
        let hasEdge = Set.member (Tuple row col) block.edges
                   || Set.member (Tuple col row) block.edges
        in if hasEdge || row == col
          then Just $ elem Rect
            [ staticNum "x" (offset + toNumber ci * cellSize)
            , staticNum "y" (offset + toNumber ri * cellSize)
            , staticNum "width" (cellSize - 1.0)
            , staticNum "height" (cellSize - 1.0)
            , staticStr "fill" (if row == col then pal.light else pal.fill)
            , staticStr "rx" "1"
            ]
            []
          else Nothing
      ) (mapWithIndex Tuple nodes)
    ) (mapWithIndex Tuple nodes)
    -- Row/col labels
    labels = Array.concatMap (\(Tuple i node) ->
      [ elem Text
          [ staticNum "x" (offset - 4.0)
          , staticNum "y" (offset + toNumber i * cellSize + cellSize / 2.0 + 1.0)
          , staticStr "text-anchor" "end"
          , staticStr "font-size" "8"
          , staticStr "fill" "#666"
          , staticStr "textContent" node
          ]
          []
      , elem Text
          [ staticNum "x" (offset + toNumber i * cellSize + cellSize / 2.0)
          , staticNum "y" (offset - 4.0)
          , staticStr "text-anchor" "middle"
          , staticStr "font-size" "8"
          , staticStr "fill" "#666"
          , staticStr "textContent" node
          ]
          []
      ]
    ) (mapWithIndex Tuple nodes)
  in
    elem Group [] (cells <> labels)

-- | Bipartite block → two columns with bands
renderBipartite :: DecompInfo -> BlockInfo -> PaletteEntry -> Tree
renderBipartite info block pal =
  case Map.lookup block.index info.bipartite of
    Just (Right parts) ->
      let
        leftNodes = sortBy compare (Set.toUnfoldable parts.partA :: Array String)
        rightNodes = sortBy compare (Set.toUnfoldable parts.partB :: Array String)
        leftN = length leftNodes
        rightN = length rightNodes
        maxN = max leftN rightN
        spacing = min 25.0 (200.0 / toNumber maxN)
        leftX = -50.0
        rightX = 50.0
        topY = -(toNumber maxN * spacing) / 2.0
        -- Left column nodes
        leftElems = mapWithIndex (\i node ->
          let y = topY + toNumber i * spacing + spacing / 2.0
          in elem Group []
            [ elem Circle
                [ staticNum "cx" leftX
                , staticNum "cy" y
                , staticNum "r" 8.0
                , staticStr "fill" pal.fill
                , staticStr "stroke" pal.stroke
                , staticStr "stroke-width" "1.5"
                ]
                []
            , elem Text
                [ staticNum "x" (leftX - 14.0)
                , staticNum "y" (y + 1.0)
                , staticStr "text-anchor" "end"
                , staticStr "font-size" "9"
                , staticStr "fill" "#666"
                , staticStr "textContent" node
                ]
                []
            ]
        ) leftNodes
        -- Right column nodes
        rightElems = mapWithIndex (\i node ->
          let y = topY + toNumber i * spacing + spacing / 2.0
          in elem Group []
            [ elem Circle
                [ staticNum "cx" rightX
                , staticNum "cy" y
                , staticNum "r" 8.0
                , staticStr "fill" pal.light
                , staticStr "stroke" pal.stroke
                , staticStr "stroke-width" "1.5"
                ]
                []
            , elem Text
                [ staticNum "x" (rightX + 14.0)
                , staticNum "y" (y + 1.0)
                , staticStr "text-anchor" "start"
                , staticStr "font-size" "9"
                , staticStr "fill" "#666"
                , staticStr "textContent" node
                ]
                []
            ]
        ) rightNodes
        -- Bands between connected pairs
        bands = Array.concatMap (\(Tuple li lNode) ->
          Array.mapMaybe (\(Tuple ri rNode) ->
            if Set.member (Tuple lNode rNode) block.edges
               || Set.member (Tuple rNode lNode) block.edges
            then
              let ly = topY + toNumber li * spacing + spacing / 2.0
                  ry = topY + toNumber ri * spacing + spacing / 2.0
              in Just $ elem Path
                [ staticStr "d" (
                    "M" <> show (leftX + 8.0) <> "," <> show (ly - 2.0)
                    <> "C" <> show 0.0 <> "," <> show ly
                    <> " " <> show 0.0 <> "," <> show ry
                    <> " " <> show (rightX - 8.0) <> "," <> show (ry - 2.0)
                    <> "L" <> show (rightX - 8.0) <> "," <> show (ry + 2.0)
                    <> "C" <> show 0.0 <> "," <> show ry
                    <> " " <> show 0.0 <> "," <> show ly
                    <> " " <> show (leftX + 8.0) <> "," <> show (ly + 2.0)
                    <> "Z"
                  )
                , staticStr "fill" pal.fill
                , staticStr "opacity" "0.25"
                ]
                []
            else Nothing
          ) (mapWithIndex Tuple rightNodes)
        ) (mapWithIndex Tuple leftNodes)
      in
        elem Group [] (bands <> leftElems <> rightElems)
    _ -> renderNodeLink block pal  -- fallback

-- | Cycle → arc diagram (nodes on a line, arcs above)
renderArc :: BlockInfo -> PaletteEntry -> Tree
renderArc block pal =
  let
    nodes = sortBy compare (Set.toUnfoldable block.nodes :: Array String)
    n = length nodes
    spacing = min 30.0 (160.0 / toNumber (max 1 (n - 1)))
    totalW = toNumber (n - 1) * spacing
    startX = -totalW / 2.0
    nodeY = 30.0
    -- Position map
    posMap = foldl (\m (Tuple i node) ->
      Map.insert node (startX + toNumber i * spacing) m
    ) Map.empty (mapWithIndex Tuple nodes)
    -- Arcs
    arcs = Array.mapMaybe (\(Tuple a b) ->
      case Map.lookup a posMap, Map.lookup b posMap of
        Just ax, Just bx ->
          let
            midX = (ax + bx) / 2.0
            radius = abs (bx - ax) / 2.0
            arcY = nodeY - radius * 0.8
          in Just $ elem Path
            [ staticStr "d" (
                "M" <> show ax <> "," <> show nodeY
                <> "Q" <> show midX <> "," <> show arcY
                <> " " <> show bx <> "," <> show nodeY
              )
            , staticStr "fill" "none"
            , staticStr "stroke" pal.fill
            , staticStr "stroke-width" "2"
            , staticStr "stroke-opacity" "0.5"
            ]
            []
        _, _ -> Nothing
    ) (Set.toUnfoldable block.edges :: Array (Tuple String String))
    -- Nodes
    nodeElems = mapWithIndex (\i node ->
      let x = startX + toNumber i * spacing
      in elem Group []
        [ elem Circle
            [ staticNum "cx" x
            , staticNum "cy" nodeY
            , staticNum "r" 7.0
            , staticStr "fill" pal.fill
            , staticStr "stroke" pal.stroke
            , staticStr "stroke-width" "1.5"
            ]
            []
        , elem Text
            [ staticNum "x" x
            , staticNum "y" (nodeY + 18.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "8"
            , staticStr "fill" "#666"
            , staticStr "textContent" node
            ]
            []
        ]
    ) nodes
  in
    elem Group [] (arcs <> nodeElems)

-- | Sparse block → simple circular node-link
renderNodeLink :: BlockInfo -> PaletteEntry -> Tree
renderNodeLink block pal =
  let
    nodes = sortBy compare (Set.toUnfoldable block.nodes :: Array String)
    n = length nodes
    radius = min 60.0 (toNumber n * 12.0)
    -- Circular layout
    posMap = foldl (\m (Tuple i node) ->
      let angle = 2.0 * pi * toNumber i / toNumber n - pi / 2.0
          x = radius * cos angle
          y = radius * sin angle
      in Map.insert node { x, y } m
    ) Map.empty (mapWithIndex Tuple nodes)
    -- Edges
    edgeElems = Array.mapMaybe (\(Tuple a b) ->
      case Map.lookup a posMap, Map.lookup b posMap of
        Just pa, Just pb ->
          Just $ elem Line
            [ staticNum "x1" pa.x
            , staticNum "y1" pa.y
            , staticNum "x2" pb.x
            , staticNum "y2" pb.y
            , staticStr "stroke" pal.stroke
            , staticStr "stroke-width" "1.5"
            , staticStr "stroke-opacity" "0.4"
            ]
            []
        _, _ -> Nothing
    ) (Set.toUnfoldable block.edges :: Array (Tuple String String))
    -- Nodes
    nodeElems = map (\node ->
      case Map.lookup node posMap of
        Just pos ->
          elem Group []
            [ elem Circle
                [ staticNum "cx" pos.x
                , staticNum "cy" pos.y
                , staticNum "r" 7.0
                , staticStr "fill" pal.fill
                , staticStr "stroke" pal.stroke
                , staticStr "stroke-width" "1.5"
                ]
                []
            , elem Text
                [ staticNum "x" pos.x
                , staticNum "y" (pos.y + 18.0)
                , staticStr "text-anchor" "middle"
                , staticStr "font-size" "8"
                , staticStr "fill" "#666"
                , staticStr "textContent" node
                ]
                []
            ]
        Nothing -> empty
    ) nodes
  in
    elem Group [] (edgeElems <> nodeElems)

-- =============================================================================
-- Chimera: Chord — dense blocks as chord diagrams, cycles as rings
-- =============================================================================

chimeraChordTree :: DecompInfo -> Tree
chimeraChordTree info =
  let
    realBlocks = Array.filter (\b -> not b.isBridge) info.blocks
    sorted = sortBy (\a b -> compare (Set.size b.nodes) (Set.size a.nodes)) realBlocks
    n = length sorted
    blockWidth = 180.0
    totalWidth = toNumber n * blockWidth
    startX = (900.0 - totalWidth) / 2.0
    blockTrees = mapWithIndex (\i block ->
      let
        x = startX + toNumber i * blockWidth
        shape = classifyBlock info block
        pal = paletteFor block.index
      in
        elem Group
          [ staticStr "transform" ("translate(" <> show (x + blockWidth / 2.0) <> ",140)")
          ]
          [ elem Text
              [ staticNum "x" 0.0, staticNum "y" (-120.0)
              , staticStr "text-anchor" "middle"
              , staticStr "font-size" "11", staticStr "font-weight" "600"
              , staticStr "fill" pal.fill
              , staticStr "textContent" (shapeLabel shape)
              ] []
          , renderBlockChord info block shape pal
          , elem Text
              [ staticNum "x" 0.0, staticNum "y" 125.0
              , staticStr "text-anchor" "middle"
              , staticStr "font-size" "10", staticStr "fill" "#999"
              , staticStr "textContent" (show (Set.size block.nodes) <> " nodes")
              ] []
          ]
    ) sorted
    connectors = if n > 1
      then [ elem Line
               [ staticNum "x1" (startX + blockWidth / 2.0), staticNum "y1" 140.0
               , staticNum "x2" (startX + toNumber (n - 1) * blockWidth + blockWidth / 2.0), staticNum "y2" 140.0
               , staticStr "stroke" "#ccc", staticStr "stroke-width" "1", staticStr "stroke-dasharray" "4,4"
               ] []
           ]
      else []
  in
    elem SVG
      [ staticStr "viewBox" "0 0 900 280"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      ]
      (connectors <> blockTrees)

renderBlockChord :: DecompInfo -> BlockInfo -> BlockShape -> PaletteEntry -> Tree
renderBlockChord info block shape pal = case shape of
  ShapeDense -> renderChord block pal
  ShapeCycle -> renderRing block pal
  ShapeBipartite -> renderBipartite info block pal
  ShapeSparse -> renderNodeLink block pal
  ShapeTree -> empty

-- | Dense block as chord diagram: nodes on circle, chords between connected pairs
renderChord :: BlockInfo -> PaletteEntry -> Tree
renderChord block pal =
  let
    nodes = sortBy compare (Set.toUnfoldable block.nodes :: Array String)
    n = length nodes
    radius = min 70.0 (toNumber n * 15.0)
    labelR = radius + 16.0
    -- Node positions on circle
    posMap = foldl (\m (Tuple i node) ->
      let angle = 2.0 * pi * toNumber i / toNumber n - pi / 2.0
      in Map.insert node { x: radius * cos angle, y: radius * sin angle, angle } m
    ) Map.empty (mapWithIndex Tuple nodes)
    -- Chords (quadratic bezier through center-ish)
    chords = Array.mapMaybe (\(Tuple a b) ->
      case Map.lookup a posMap, Map.lookup b posMap of
        Just pa, Just pb ->
          -- Control point pulled toward center for nice curvature
          let cx = (pa.x + pb.x) * 0.15
              cy = (pa.y + pb.y) * 0.15
          in Just $ elem Path
            [ staticStr "d" (
                "M" <> show pa.x <> "," <> show pa.y
                <> "Q" <> show cx <> "," <> show cy
                <> " " <> show pb.x <> "," <> show pb.y
              )
            , staticStr "fill" "none"
            , staticStr "stroke" pal.fill
            , staticStr "stroke-width" "1.5"
            , staticStr "stroke-opacity" "0.4"
            ]
            []
        _, _ -> Nothing
    ) (Set.toUnfoldable block.edges :: Array (Tuple String String))
    -- Nodes on circle
    nodeElems = map (\node ->
      case Map.lookup node posMap of
        Just pos ->
          elem Group []
            [ elem Circle
                [ staticNum "cx" pos.x, staticNum "cy" pos.y, staticNum "r" 6.0
                , staticStr "fill" pal.fill, staticStr "stroke" "#fff", staticStr "stroke-width" "2"
                ] []
            , elem Text
                [ staticNum "x" (labelR * cos pos.angle), staticNum "y" (labelR * sin pos.angle + 1.0)
                , staticStr "text-anchor" (if cos pos.angle > 0.3 then "start" else if cos pos.angle < -0.3 then "end" else "middle")
                , staticStr "font-size" "8", staticStr "fill" "#666"
                , staticStr "textContent" node
                ] []
            ]
        Nothing -> empty
    ) nodes
    -- Outer ring
    ring = elem Circle
      [ staticNum "cx" 0.0, staticNum "cy" 0.0, staticNum "r" radius
      , staticStr "fill" "none", staticStr "stroke" pal.light, staticStr "stroke-width" "1"
      ] []
  in
    elem Group [] ([ring] <> chords <> nodeElems)

-- | Cycle block as a ring: nodes placed on circle, edges drawn as arcs along the ring
renderRing :: BlockInfo -> PaletteEntry -> Tree
renderRing block pal =
  let
    nodes = sortBy compare (Set.toUnfoldable block.nodes :: Array String)
    n = length nodes
    radius = min 55.0 (toNumber n * 14.0)
    labelR = radius + 14.0
    -- Walk the cycle to order nodes correctly
    orderedNodes = walkCycle nodes block.edges
    posMap = foldl (\m (Tuple i node) ->
      let angle = 2.0 * pi * toNumber i / toNumber n - pi / 2.0
      in Map.insert node { x: radius * cos angle, y: radius * sin angle, angle } m
    ) Map.empty (mapWithIndex Tuple orderedNodes)
    -- Ring path (closed circle through all nodes)
    ringPath = elem Circle
      [ staticNum "cx" 0.0, staticNum "cy" 0.0, staticNum "r" radius
      , staticStr "fill" "none"
      , staticStr "stroke" pal.fill, staticStr "stroke-width" "3"
      , staticStr "stroke-opacity" "0.3"
      ] []
    -- Edge arcs along the ring
    edgeArcs = Array.mapMaybe (\(Tuple a b) ->
      case Map.lookup a posMap, Map.lookup b posMap of
        Just pa, Just pb ->
          Just $ elem Line
            [ staticNum "x1" pa.x, staticNum "y1" pa.y
            , staticNum "x2" pb.x, staticNum "y2" pb.y
            , staticStr "stroke" pal.fill, staticStr "stroke-width" "2.5", staticStr "stroke-opacity" "0.5"
            ] []
        _, _ -> Nothing
    ) (Set.toUnfoldable block.edges :: Array (Tuple String String))
    -- Nodes
    nodeElems = map (\node ->
      case Map.lookup node posMap of
        Just pos ->
          elem Group []
            [ elem Circle
                [ staticNum "cx" pos.x, staticNum "cy" pos.y, staticNum "r" 7.0
                , staticStr "fill" pal.fill, staticStr "stroke" "#fff", staticStr "stroke-width" "2"
                ] []
            , elem Text
                [ staticNum "x" (labelR * cos pos.angle), staticNum "y" (labelR * sin pos.angle + 1.0)
                , staticStr "text-anchor" (if cos pos.angle > 0.3 then "start" else if cos pos.angle < -0.3 then "end" else "middle")
                , staticStr "font-size" "9", staticStr "fill" "#666"
                , staticStr "textContent" node
                ] []
            ]
        Nothing -> empty
    ) orderedNodes
  in
    elem Group [] ([ringPath] <> edgeArcs <> nodeElems)

-- | Walk a cycle's edges to produce ordered nodes
walkCycle :: Array String -> Set (Tuple String String) -> Array String
walkCycle nodes edges =
  let
    -- Build adjacency for the cycle
    adj = foldl (\m (Tuple a b) ->
      Map.alter (Just <<< Set.insert b <<< fromMaybe Set.empty) a
        (Map.alter (Just <<< Set.insert a <<< fromMaybe Set.empty) b m)
    ) Map.empty (Set.toUnfoldable edges :: Array (Tuple String String))
    -- Start from first node, greedily walk
    start = fromMaybe "" (nodes !! 0)
    walk visited current result =
      let neighbors = fromMaybe Set.empty $ Map.lookup current adj
          next = Array.find (\n -> not (Set.member n visited)) (Set.toUnfoldable neighbors :: Array String)
      in case next of
        Just n -> walk (Set.insert n visited) n (result <> [n])
        Nothing -> result
  in
    [start] <> walk (Set.singleton start) start []

-- =============================================================================
-- Chimera: Spine — block-cut tree as horizontal skeleton
-- =============================================================================

chimeraSpineTree :: SimpleGraph String -> DecompInfo -> Tree
chimeraSpineTree graph info =
  let
    bct = Dec.blockCutTree graph
    nBlocks = length bct.blocks
    apArray = Set.toUnfoldable bct.cutVertices :: Array String

    -- Assign horizontal positions: lay out block-cut tree nodes along x-axis
    -- Use simple left-to-right ordering based on tree traversal
    nodePositions = assignSpinePositions bct

    -- Render each block at its position
    blockTrees = Array.concatMap (\(Tuple blockIdx block) ->
      case Map.lookup blockIdx nodePositions of
        Just pos ->
          let
            blockInfo = fromMaybe { index: blockIdx, nodes: block, edges: Set.empty, isBridge: false }
                          (Array.find (\b -> b.index == blockIdx) info.blocks)
            shape = classifyBlock info blockInfo
            pal = paletteFor blockIdx
            scale = if Set.size block <= 2 then 0.5 else 0.8
          in
            [ elem Group
                [ staticStr "transform" ("translate(" <> show pos.x <> "," <> show pos.y <> ") scale(" <> show scale <> ")") ]
                [ renderBlockChord info blockInfo shape pal ]
            ]
        Nothing -> []
    ) (mapWithIndex Tuple bct.blocks)

    -- Render cut vertices as diamonds
    apElems = Array.concatMap (\(Tuple i ap) ->
      let apIdx = nBlocks + i
      in case Map.lookup apIdx nodePositions of
        Just pos ->
          [ elem Polygon
              [ staticStr "points" (diamondPoints pos.x pos.y 8.0)
              , staticStr "fill" "#fff", staticStr "stroke" "#333", staticStr "stroke-width" "2"
              ] []
          , elem Text
              [ staticNum "x" pos.x, staticNum "y" (pos.y + 20.0)
              , staticStr "text-anchor" "middle", staticStr "font-size" "9"
              , staticStr "font-weight" "600", staticStr "fill" "#333"
              , staticStr "textContent" ap
              ] []
          ]
        Nothing -> []
    ) (mapWithIndex Tuple apArray)

    -- Render tree edges (connections between blocks and APs)
    edgeElems = Array.mapMaybe (\e ->
      case Map.lookup e.from nodePositions, Map.lookup e.to nodePositions of
        Just pa, Just pb ->
          Just $ elem Path
            [ staticStr "d" (
                "M" <> show pa.x <> "," <> show pa.y
                <> "C" <> show ((pa.x + pb.x) / 2.0) <> "," <> show pa.y
                <> " " <> show ((pa.x + pb.x) / 2.0) <> "," <> show pb.y
                <> " " <> show pb.x <> "," <> show pb.y
              )
            , staticStr "fill" "none"
            , staticStr "stroke" "#999"
            , staticStr "stroke-width" "2"
            , staticStr "stroke-opacity" "0.4"
            , staticStr "stroke-dasharray" "4,3"
            ]
            []
        _, _ -> Nothing
    ) bct.tree
  in
    elem SVG
      [ staticStr "viewBox" "0 0 900 320"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      ]
      (edgeElems <> blockTrees <> apElems)

-- | Assign positions to block-cut tree nodes along a horizontal spine
assignSpinePositions
  :: { blocks :: Array (Set String), cutVertices :: Set String, tree :: Array { from :: Int, to :: Int } }
  -> Map Int { x :: Number, y :: Number }
assignSpinePositions bct =
  let
    -- Build adjacency for the tree
    adj = foldl (\m e ->
      Map.alter (Just <<< Set.insert e.to <<< fromMaybe Set.empty) e.from
        (Map.alter (Just <<< Set.insert e.from <<< fromMaybe Set.empty) e.to m)
    ) Map.empty bct.tree

    -- BFS from node 0 to get ordering
    bfsOrder = bfs adj 0
    n = length bfsOrder
    spacing = if n > 1 then 800.0 / toNumber (n - 1) else 0.0
    startX = 50.0

    -- Position each node along x-axis, blocks at y=160, APs at y=160 too (with offsets for visual separation)
    positions = foldl (\m (Tuple i nodeIdx) ->
      let x = startX + toNumber i * spacing
      in Map.insert nodeIdx { x, y: 160.0 } m
    ) Map.empty (mapWithIndex Tuple bfsOrder)
  in
    positions

-- | Simple BFS traversal
bfs :: Map Int (Set Int) -> Int -> Array Int
bfs adj start = go [start] (Set.singleton start) [start]
  where
  go queue visited result = case Array.uncons queue of
    Nothing -> result
    Just { head, tail } ->
      let neighbors = Set.toUnfoldable (fromMaybe Set.empty $ Map.lookup head adj) :: Array Int
          newNeighbors = Array.filter (\n -> not (Set.member n visited)) neighbors
          newVisited = foldl (flip Set.insert) visited newNeighbors
      in go (tail <> newNeighbors) newVisited (result <> newNeighbors)

-- =============================================================================
-- Chimera: Orbital — cycles as rings, cliques as suns, connected through space
-- =============================================================================

chimeraOrbitalTree :: SimpleGraph String -> DecompInfo -> Tree
chimeraOrbitalTree graph info =
  let
    bct = Dec.blockCutTree graph
    nBlocks = length bct.blocks
    apArray = Set.toUnfoldable bct.cutVertices :: Array String

    -- Get positions from spine layout but with more vertical spread
    adj = foldl (\m e ->
      Map.alter (Just <<< Set.insert e.to <<< fromMaybe Set.empty) e.from
        (Map.alter (Just <<< Set.insert e.from <<< fromMaybe Set.empty) e.to m)
    ) Map.empty bct.tree

    bfsOrder = bfs adj 0
    n = length bfsOrder
    -- Arrange in a gentle arc
    positions = foldl (\m (Tuple i nodeIdx) ->
      let t = if n > 1 then toNumber i / toNumber (n - 1) else 0.5
          x = 80.0 + t * 740.0
          -- Gentle sine wave for vertical variation
          y = 160.0 + 60.0 * sin (t * pi)
      in Map.insert nodeIdx { x, y } m
    ) Map.empty (mapWithIndex Tuple bfsOrder)

    -- Render blocks as orbital bodies
    blockTrees = Array.concatMap (\(Tuple blockIdx block) ->
      case Map.lookup blockIdx positions of
        Just pos ->
          let
            blockInfo = fromMaybe { index: blockIdx, nodes: block, edges: Set.empty, isBridge: false }
                          (Array.find (\b -> b.index == blockIdx) info.blocks)
            shape = classifyBlock info blockInfo
            pal = paletteFor blockIdx
            nodeCount = Set.size block
            -- Scale based on block size
            r = toNumber nodeCount * 6.0 + 10.0
          in
            [ renderOrbitalBlock pos shape blockInfo pal r info ]
        Nothing -> []
    ) (mapWithIndex Tuple bct.blocks)

    -- Render cut vertices as junction diamonds
    apElems = Array.concatMap (\(Tuple i ap) ->
      let apIdx = nBlocks + i
      in case Map.lookup apIdx positions of
        Just pos ->
          [ elem Polygon
              [ staticStr "points" (diamondPoints pos.x pos.y 7.0)
              , staticStr "fill" "#fff", staticStr "stroke" "#333", staticStr "stroke-width" "2"
              ] []
          , elem Text
              [ staticNum "x" pos.x, staticNum "y" (pos.y - 12.0)
              , staticStr "text-anchor" "middle", staticStr "font-size" "8"
              , staticStr "font-weight" "600", staticStr "fill" "#333"
              , staticStr "textContent" ap
              ] []
          ]
        Nothing -> []
    ) (mapWithIndex Tuple apArray)

    -- Render connections
    edgeElems = Array.mapMaybe (\e ->
      case Map.lookup e.from positions, Map.lookup e.to positions of
        Just pa, Just pb ->
          Just $ elem Line
            [ staticNum "x1" pa.x, staticNum "y1" pa.y
            , staticNum "x2" pb.x, staticNum "y2" pb.y
            , staticStr "stroke" "#bbb", staticStr "stroke-width" "1.5"
            , staticStr "stroke-dasharray" "3,3"
            ] []
        _, _ -> Nothing
    ) bct.tree
  in
    elem SVG
      [ staticStr "viewBox" "0 0 900 320"
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      ]
      (edgeElems <> blockTrees <> apElems)

-- | Render a single block as an orbital body — form matches structure
renderOrbitalBlock :: { x :: Number, y :: Number } -> BlockShape -> BlockInfo -> PaletteEntry -> Number -> DecompInfo -> Tree
renderOrbitalBlock pos shape block pal r info =
  let
    nodes = sortBy compare (Set.toUnfoldable block.nodes :: Array String)
    n = length nodes
  in case shape of
    ShapeDense ->
      -- Filled circle with internal chord lines
      let chordR = r * 0.8
          posMap = foldl (\m (Tuple i node) ->
            let angle = 2.0 * pi * toNumber i / toNumber n - pi / 2.0
            in Map.insert node { x: pos.x + chordR * cos angle, y: pos.y + chordR * sin angle } m
          ) Map.empty (mapWithIndex Tuple nodes)
          chords = Array.mapMaybe (\(Tuple a b) ->
            case Map.lookup a posMap, Map.lookup b posMap of
              Just pa, Just pb ->
                Just $ elem Line
                  [ staticNum "x1" pa.x, staticNum "y1" pa.y
                  , staticNum "x2" pb.x, staticNum "y2" pb.y
                  , staticStr "stroke" pal.fill, staticStr "stroke-width" "1.5", staticStr "stroke-opacity" "0.5"
                  ] []
              _, _ -> Nothing
          ) (Set.toUnfoldable block.edges :: Array (Tuple String String))
          bg = elem Circle
            [ staticNum "cx" pos.x, staticNum "cy" pos.y, staticNum "r" r
            , staticStr "fill" pal.light, staticStr "fill-opacity" "0.4"
            , staticStr "stroke" pal.fill, staticStr "stroke-width" "2", staticStr "stroke-opacity" "0.6"
            ] []
          dots = map (\node ->
            case Map.lookup node posMap of
              Just p -> elem Circle
                [ staticNum "cx" p.x, staticNum "cy" p.y, staticNum "r" 3.0
                , staticStr "fill" pal.fill, staticStr "stroke" "#fff", staticStr "stroke-width" "1"
                ] []
              Nothing -> empty
          ) nodes
          label = elem Text
            [ staticNum "x" pos.x, staticNum "y" (pos.y + r + 14.0)
            , staticStr "text-anchor" "middle", staticStr "font-size" "9"
            , staticStr "fill" pal.fill, staticStr "font-weight" "600"
            , staticStr "textContent" "dense"
            ] []
      in elem Group [] ([bg] <> chords <> dots <> [label])

    ShapeCycle ->
      -- Ring shape — circle outline with nodes on it
      let ringR = r * 0.75
          orderedNodes = walkCycle nodes block.edges
          posMap = foldl (\m (Tuple i node) ->
            let angle = 2.0 * pi * toNumber i / toNumber n - pi / 2.0
            in Map.insert node { x: pos.x + ringR * cos angle, y: pos.y + ringR * sin angle } m
          ) Map.empty (mapWithIndex Tuple orderedNodes)
          ring = elem Circle
            [ staticNum "cx" pos.x, staticNum "cy" pos.y, staticNum "r" ringR
            , staticStr "fill" "none"
            , staticStr "stroke" pal.fill, staticStr "stroke-width" "3", staticStr "stroke-opacity" "0.3"
            ] []
          dots = map (\node ->
            case Map.lookup node posMap of
              Just p -> elem Circle
                [ staticNum "cx" p.x, staticNum "cy" p.y, staticNum "r" 4.0
                , staticStr "fill" pal.fill, staticStr "stroke" "#fff", staticStr "stroke-width" "1.5"
                ] []
              Nothing -> empty
          ) orderedNodes
          label = elem Text
            [ staticNum "x" pos.x, staticNum "y" (pos.y + r + 14.0)
            , staticStr "text-anchor" "middle", staticStr "font-size" "9"
            , staticStr "fill" pal.fill, staticStr "font-weight" "600"
            , staticStr "textContent" "cycle"
            ] []
      in elem Group [] ([ring] <> dots <> [label])

    ShapeBipartite ->
      -- Two parallel rows of dots
      case Map.lookup block.index info.bipartite of
        Just (Right parts) ->
          let leftN = Set.toUnfoldable parts.partA :: Array String
              rightN = Set.toUnfoldable parts.partB :: Array String
              lCount = length leftN
              rCount = length rightN
              maxN = max lCount rCount
              spread = min (r * 1.5) (toNumber maxN * 10.0)
              leftDots = mapWithIndex (\i _ ->
                let y = pos.y - spread / 2.0 + spread * toNumber i / toNumber (max 1 (lCount - 1))
                in elem Circle
                  [ staticNum "cx" (pos.x - 12.0), staticNum "cy" y, staticNum "r" 3.5
                  , staticStr "fill" pal.fill, staticStr "stroke" "#fff", staticStr "stroke-width" "1"
                  ] []
              ) leftN
              rightDots = mapWithIndex (\i _ ->
                let y = pos.y - spread / 2.0 + spread * toNumber i / toNumber (max 1 (rCount - 1))
                in elem Circle
                  [ staticNum "cx" (pos.x + 12.0), staticNum "cy" y, staticNum "r" 3.5
                  , staticStr "fill" pal.light, staticStr "stroke" pal.stroke, staticStr "stroke-width" "1"
                  ] []
              ) rightN
              bg = elem Rect
                [ staticNum "x" (pos.x - r), staticNum "y" (pos.y - r)
                , staticNum "width" (r * 2.0), staticNum "height" (r * 2.0)
                , staticStr "fill" pal.light, staticStr "fill-opacity" "0.2"
                , staticStr "stroke" pal.fill, staticStr "stroke-width" "1", staticStr "stroke-opacity" "0.3"
                , staticStr "rx" "4"
                ] []
              label = elem Text
                [ staticNum "x" pos.x, staticNum "y" (pos.y + r + 14.0)
                , staticStr "text-anchor" "middle", staticStr "font-size" "9"
                , staticStr "fill" pal.fill, staticStr "font-weight" "600"
                , staticStr "textContent" "bipartite"
                ] []
          in elem Group [] ([bg] <> leftDots <> rightDots <> [label])
        _ -> renderOrbitalBlock pos ShapeSparse block pal r info

    ShapeSparse ->
      -- Simple cluster of dots
      let posMap = foldl (\m (Tuple i node) ->
            let angle = 2.0 * pi * toNumber i / toNumber n - pi / 2.0
                nr = r * 0.6
            in Map.insert node { x: pos.x + nr * cos angle, y: pos.y + nr * sin angle } m
          ) Map.empty (mapWithIndex Tuple nodes)
          edges = Array.mapMaybe (\(Tuple a b) ->
            case Map.lookup a posMap, Map.lookup b posMap of
              Just pa, Just pb ->
                Just $ elem Line
                  [ staticNum "x1" pa.x, staticNum "y1" pa.y
                  , staticNum "x2" pb.x, staticNum "y2" pb.y
                  , staticStr "stroke" pal.stroke, staticStr "stroke-width" "1", staticStr "stroke-opacity" "0.3"
                  ] []
              _, _ -> Nothing
          ) (Set.toUnfoldable block.edges :: Array (Tuple String String))
          dots = map (\node ->
            case Map.lookup node posMap of
              Just p -> elem Circle
                [ staticNum "cx" p.x, staticNum "cy" p.y, staticNum "r" 3.5
                , staticStr "fill" pal.fill, staticStr "stroke" "#fff", staticStr "stroke-width" "1"
                ] []
              Nothing -> empty
          ) nodes
          label = elem Text
            [ staticNum "x" pos.x, staticNum "y" (pos.y + r + 14.0)
            , staticStr "text-anchor" "middle", staticStr "font-size" "9"
            , staticStr "fill" pal.fill, staticStr "font-weight" "600"
            , staticStr "textContent" "sparse"
            ] []
      in elem Group [] (edges <> dots <> [label])

    ShapeTree ->
      -- Bridge: small dot with label
      elem Group []
        [ elem Circle
            [ staticNum "cx" pos.x, staticNum "cy" pos.y, staticNum "r" 4.0
            , staticStr "fill" "#999", staticStr "stroke" "#666", staticStr "stroke-width" "1"
            ] []
        ]

-- =============================================================================
-- Halogen helpers (metrics panel, legend — stay as HTML)
-- =============================================================================

renderMetrics :: forall w i. DecompInfo -> HH.HTML w i
renderMetrics info =
  HH.div [ HP.class_ (HH.ClassName "metrics-panel") ]
    [ metricRow "Biconnected components" (show (length info.blocks))
    , metricRow "Articulation points" (show info.metrics.articulationPointCount)
    , metricRow "Bridges" (show info.metrics.bridgeCount)
    , metricRow "Bipartite" (if info.metrics.isBipartite then "yes" else "no")
    , metricRow "Tree" (if info.metrics.isTree then "yes" else "no")
    , metricRow "Largest block" (show info.metrics.maxBlockSize <> " nodes")
    , metricRow "Treelikeness" (showPercent info.metrics.treelikeness)
    ]

metricRow :: forall w i. String -> String -> HH.HTML w i
metricRow label value =
  HH.div [ HP.class_ (HH.ClassName "metric-row") ]
    [ HH.span [ HP.class_ (HH.ClassName "metric-label") ] [ HH.text label ]
    , HH.span [ HP.class_ (HH.ClassName "metric-value") ] [ HH.text value ]
    ]

showPercent :: Number -> String
showPercent n =
  let pct = n * 100.0
  in show (roundTo1 pct) <> "%"

foreign import roundTo1 :: Number -> Number

renderLegend :: forall w i. HH.HTML w i
renderLegend =
  HH.div [ HP.class_ (HH.ClassName "decomp-legend") ]
    [ legendItem "#fff" "#333" true "Articulation point"
    , legendItem "#888" "#888" false "Bridge (dashed)"
    , HH.div [ HP.class_ (HH.ClassName "legend-sep") ] []
    , HH.span [ HP.class_ (HH.ClassName "legend-note") ]
        [ HH.text "Colors = biconnected components" ]
    ]

legendItem :: forall w i. String -> String -> Boolean -> String -> HH.HTML w i
legendItem fill stroke isDiamond label =
  HH.div [ HP.class_ (HH.ClassName "legend-item") ]
    [ HH.div
        [ HP.class_ (HH.ClassName $ "legend-swatch" <> if isDiamond then " diamond" else "")
        , HP.style ("background:" <> fill <> ";border:2px solid " <> stroke)
        ]
        []
    , HH.text label
    ]

-- | Chimera legend showing block→form mapping
renderChimeraLegend :: forall w i. DecompInfo -> HH.HTML w i
renderChimeraLegend info =
  let
    realBlocks = Array.filter (\b -> not b.isBridge) info.blocks
    items = map (\block ->
      let shape = classifyBlock info block
          pal = paletteFor block.index
          n = Set.size block.nodes
      in HH.div [ HP.class_ (HH.ClassName "chimera-legend-item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "legend-swatch")
            , HP.style ("background:" <> pal.fill)
            ]
            []
        , HH.text (show n <> " nodes → " <> shapeLabel shape)
        ]
    ) realBlocks
  in
    HH.div [ HP.class_ (HH.ClassName "chimera-legend") ] items

-- Alt helper
infixl 3 alt as <|>

alt :: forall a. Maybe a -> Maybe a -> Maybe a
alt (Just x) _ = Just x
alt Nothing y = y

abs :: Number -> Number
abs x = if x < 0.0 then -x else x
