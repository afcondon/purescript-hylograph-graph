-- | Hand-crafted showcase graphs for decomposition demo.
-- |
-- | Each graph is designed to have structurally diverse regions that
-- | decompose into clearly different biconnected components — the kind
-- | of structure that motivates chimera visualizations.
module Decomposition.Graphs where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Array (foldl)
import Data.Graph.Algorithms (SimpleGraph)

type Pos = { x :: Number, y :: Number }

type ShowcaseGraph =
  { graph :: SimpleGraph String
  , positions :: Map String Pos
  , title :: String
  , description :: String
  }

-- =============================================================================
-- Graph 1: "Chimera" — the money graph
-- =============================================================================
-- Layout (left to right):
--   [K5 clique] --cut(e)-- [tree branch] --cut(j)-- [bipartite 3×3] --cut(p)-- [path + cycle]
--
-- This graph has every structural type:
--   - Dense clique (→ matrix/chord)
--   - Tree region (→ dendrogram)
--   - Bipartite section (→ Sankey)
--   - Path with terminal cycle (→ arc diagram)

chimeraGraph :: ShowcaseGraph
chimeraGraph =
  { graph: mkUndirected nodes edges
  , positions: Map.fromFoldable positions
  , title: "Chimera"
  , description: "Dense clique → tree → bipartite → path+cycle"
  }
  where
  -- Node names deliberately scattered across blocks so alphabetical
  -- order interleaves them (maximizing the before/after matrix contrast).
  -- K5 clique: c,e,h,l,q    Tree: a,b,f,j,n    Bipartite: d,g,k,o,p
  -- Path+cycle: i,m,r,s,t,u
  nodes =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"
    , "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u"
    ]

  edges =
    -- K5 clique {c,e,h,l,q}
    [ t "c" "h", t "c" "l", t "c" "q", t "c" "e"
    , t "h" "l", t "h" "q", t "h" "e"
    , t "l" "q", t "l" "e"
    , t "q" "e"
    -- Bridge: e-a (K5 cut → tree hub)
    , t "e" "a"
    -- Tree: a is hub, j branches to b (cut → bipartite)
    , t "a" "j", t "a" "n", t "a" "f", t "j" "b"
    -- Bipartite: {b,d,o} × {g,k,p}
    , t "b" "g", t "b" "k", t "b" "p"
    , t "d" "g", t "d" "k", t "d" "p"
    , t "o" "g", t "o" "k", t "o" "p"
    -- Bridge: p-r (bipartite cut → path)
    , t "p" "r"
    -- Path: r-i-m-s
    , t "r" "i", t "i" "m", t "m" "s"
    -- Terminal triangle: s-t-u-s
    , t "s" "t", t "t" "u", t "u" "s"
    ]

  positions =
    -- K5 clique — tight cluster, left
    [ Tuple "c" { x: 60.0,  y: 120.0 }
    , Tuple "h" { x: 120.0, y: 60.0 }
    , Tuple "l" { x: 120.0, y: 180.0 }
    , Tuple "q" { x: 40.0,  y: 200.0 }
    , Tuple "e" { x: 40.0,  y: 60.0 }
    -- Tree branch — spread out, center-left
    , Tuple "a" { x: 260.0, y: 140.0 }
    , Tuple "j" { x: 340.0, y: 100.0 }
    , Tuple "n" { x: 260.0, y: 60.0 }
    , Tuple "f" { x: 260.0, y: 220.0 }
    , Tuple "b" { x: 420.0, y: 100.0 }
    -- Bipartite — two columns, center-right
    , Tuple "d" { x: 480.0, y: 160.0 }
    , Tuple "o" { x: 480.0, y: 240.0 }
    , Tuple "g" { x: 580.0, y: 80.0 }
    , Tuple "k" { x: 580.0, y: 160.0 }
    , Tuple "p" { x: 580.0, y: 240.0 }
    , Tuple "r" { x: 660.0, y: 240.0 }
    -- Path + terminal cycle — right
    , Tuple "i" { x: 720.0, y: 200.0 }
    , Tuple "m" { x: 780.0, y: 160.0 }
    , Tuple "s" { x: 840.0, y: 130.0 }
    , Tuple "t" { x: 870.0, y: 70.0 }
    , Tuple "u" { x: 810.0, y: 60.0 }
    ]

-- =============================================================================
-- Graph 2: "Archipelago" — dense islands connected by bridges
-- =============================================================================

archipelagoGraph :: ShowcaseGraph
archipelagoGraph =
  { graph: mkUndirected nodes edges
  , positions: Map.fromFoldable positions
  , title: "Archipelago"
  , description: "Dense islands connected by single bridges"
  }
  where
  -- Node names scattered across blocks:
  -- K4-1: a,e,i,m    K5: b,f,j,n,c    Triangle: g,k,o    K4-2: d,h,l,p
  nodes =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j"
    , "k", "l", "m", "n", "o", "p"
    ]

  edges =
    -- K4 island 1 {a,e,i,m}
    [ t "a" "e", t "a" "i", t "a" "m", t "e" "i", t "e" "m", t "i" "m"
    -- Bridge 1: m-b
    , t "m" "b"
    -- K5 island 2 {b,f,j,n,c}
    , t "b" "f", t "b" "j", t "b" "n", t "b" "c"
    , t "f" "j", t "f" "n", t "f" "c"
    , t "j" "n", t "j" "c"
    , t "n" "c"
    -- Bridge 2: c-g
    , t "c" "g"
    -- Triangle island 3 {g,k,o}
    , t "g" "k", t "k" "o", t "o" "g"
    -- Bridge 3: o-d
    , t "o" "d"
    -- K4 island 4 {d,h,l,p}
    , t "d" "h", t "d" "l", t "d" "p", t "h" "l", t "h" "p", t "l" "p"
    ]

  positions =
    -- Island 1 {a,e,i,m}
    [ Tuple "a" { x: 80.0,  y: 80.0 }
    , Tuple "e" { x: 140.0, y: 60.0 }
    , Tuple "i" { x: 140.0, y: 120.0 }
    , Tuple "m" { x: 200.0, y: 90.0 }
    -- Island 2 {b,f,j,n,c}
    , Tuple "b" { x: 320.0, y: 100.0 }
    , Tuple "f" { x: 370.0, y: 50.0 }
    , Tuple "j" { x: 420.0, y: 100.0 }
    , Tuple "n" { x: 370.0, y: 150.0 }
    , Tuple "c" { x: 480.0, y: 130.0 }
    -- Island 3 {g,k,o}
    , Tuple "g" { x: 580.0, y: 100.0 }
    , Tuple "k" { x: 620.0, y: 50.0 }
    , Tuple "o" { x: 660.0, y: 110.0 }
    -- Island 4 {d,h,l,p}
    , Tuple "d" { x: 760.0, y: 80.0 }
    , Tuple "h" { x: 820.0, y: 60.0 }
    , Tuple "l" { x: 820.0, y: 120.0 }
    , Tuple "p" { x: 760.0, y: 140.0 }
    ]

-- =============================================================================
-- Graph 3: "Caterpillar" — spine with dangling cycles
-- =============================================================================

caterpillarGraph :: ShowcaseGraph
caterpillarGraph =
  { graph: mkUndirected nodes edges
  , positions: Map.fromFoldable positions
  , title: "Caterpillar"
  , description: "Linear spine with cycles hanging off it"
  }
  where
  -- Node names scattered across blocks:
  -- Spine: a,c,f,h,k,m    Tri-1 off f: d,i    Square off c: b,g,l    Tri-2 off h: e,j
  nodes =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m"
    ]

  edges =
    -- Spine: a-f-k-c-h-m
    [ t "a" "f", t "f" "k", t "k" "c", t "c" "h", t "h" "m"
    -- Triangle off f: f-d-i-f
    , t "f" "d", t "d" "i", t "i" "f"
    -- Square off c: c-b-g-l-c
    , t "c" "b", t "b" "g", t "g" "l", t "l" "c"
    -- Triangle off h: h-e-j-h
    , t "h" "e", t "e" "j", t "j" "h"
    ]

  positions =
    -- Spine — horizontal across the middle
    [ Tuple "a" { x: 80.0,  y: 130.0 }
    , Tuple "f" { x: 200.0, y: 130.0 }
    , Tuple "k" { x: 350.0, y: 130.0 }
    , Tuple "c" { x: 500.0, y: 130.0 }
    , Tuple "h" { x: 650.0, y: 130.0 }
    , Tuple "m" { x: 800.0, y: 130.0 }
    -- Triangle off f — above
    , Tuple "d" { x: 170.0, y: 50.0 }
    , Tuple "i" { x: 240.0, y: 50.0 }
    -- Square off c — below
    , Tuple "b" { x: 460.0, y: 220.0 }
    , Tuple "g" { x: 520.0, y: 280.0 }
    , Tuple "l" { x: 540.0, y: 200.0 }
    -- Triangle off h — above
    , Tuple "e" { x: 620.0, y: 50.0 }
    , Tuple "j" { x: 690.0, y: 50.0 }
    ]

-- =============================================================================
-- Helpers
-- =============================================================================

t :: String -> String -> Tuple String String
t = Tuple

mkUndirected :: Array String -> Array (Tuple String String) -> SimpleGraph String
mkUndirected nodes edges =
  { nodes
  , edges: foldl (\acc (Tuple a b) ->
      Map.alter (Just <<< Set.insert b <<< fromMaybe Set.empty) a
        (Map.alter (Just <<< Set.insert a <<< fromMaybe Set.empty) b acc)
    ) Map.empty edges
  }

allShowcaseGraphs :: Array ShowcaseGraph
allShowcaseGraphs =
  [ chimeraGraph
  , archipelagoGraph
  , caterpillarGraph
  ]
