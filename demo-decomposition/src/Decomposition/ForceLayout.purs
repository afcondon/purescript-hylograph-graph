-- | Load Les Miserables graph with force-directed layout positions.
module Decomposition.ForceLayout where

import Prelude

import Data.Array (foldl)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

import Data.Graph.Algorithms (SimpleGraph)
import Decomposition.Graphs (ShowcaseGraph)

-- FFI types
type RawNode = { id :: String, x :: Number, y :: Number }
type RawEdge = { source :: String, target :: String }
type RawGraph = { nodes :: Array RawNode, edges :: Array RawEdge }

foreign import loadMiserablesImpl :: (String -> Effect Unit) -> (RawGraph -> Effect Unit) -> Effect Unit

-- | Load miserables.json and compute force layout positions
loadMiserables :: Aff (Either String ShowcaseGraph)
loadMiserables = makeAff \cb -> do
  loadMiserablesImpl
    (\err -> cb (Right (Left err)))
    (\raw -> cb (Right (Right (rawToShowcase raw))))
  pure nonCanceler

rawToShowcase :: RawGraph -> ShowcaseGraph
rawToShowcase raw =
  { graph: mkGraph raw
  , positions: Map.fromFoldable (map (\n -> Tuple n.id { x: n.x, y: n.y }) raw.nodes)
  , title: "Les Miserables"
  , description: "77 characters, 254 co-appearance links — force-directed layout"
  }

mkGraph :: RawGraph -> SimpleGraph String
mkGraph raw =
  { nodes: map _.id raw.nodes
  , edges: foldl (\acc e ->
      Map.alter (Just <<< Set.insert e.target <<< fromMaybe Set.empty) e.source
        (Map.alter (Just <<< Set.insert e.source <<< fromMaybe Set.empty) e.target acc)
    ) Map.empty raw.edges
  }
