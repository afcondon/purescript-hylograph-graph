-- | The tree a traverser is forced to build, and everything that is not in it.
-- |
-- | A designer sees a graph, in which every edge is equally near to hand and
-- | hierarchy is a matter of taste. Anyone walking the graph cannot see it that
-- | way: a walk starts somewhere and memory is hierarchical, so what a traverser
-- | ends up holding is the shortest-path tree rooted at the start, plus a pile of
-- | edges that tree does not explain. That tree is *induced* by the graph whether
-- | the designer looked at it or not, which is why a tool that only ever draws
-- | the graph is showing the designer something other than what the walker
-- | receives.
-- |
-- | Sorting the leftover edges is where the signal is. A **back** edge is the way
-- | out, and its absence is a trap. A **forward** edge skips levels — a shortcut,
-- | directness bought at the cost of taxonomy. A **cross** edge jumps into a
-- | different subtree, which is the interlevel transition that makes
-- | where-you-are impossible to reconstruct from how-you-got-here.
-- |
-- | The vocabulary is deliberately the classical DFS edge classification applied
-- | to a *breadth-first* tree, because the question here is not "is there a
-- | cycle" but "how far from home is this, and how far back" — and only the
-- | shortest-path tree answers that.
-- |
-- | Extracted from the Glassbox state-machine demo, where it was written against
-- | `String`-keyed states while `Data.Graph.Pathfinding.bfs` was undirected and
-- | could not serve. Now that `Data.Graph.Algorithms.bfsTree` is both directed
-- | and node-polymorphic, this is a general graph concept and belongs here.
module Data.Graph.InducedTree
  ( EdgeClass(..)
  , edgeClassLabel
  , Induced
  , induce
  , classOf
  , depthOf
  , returnCostOf
  , asymmetries
  , pathFromRoot
  ) where

import Prelude

import Data.Array as Array
import Data.Graph.Algorithms (SimpleGraph, bfsTree, getAllEdges, reverseGraph, treePath)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

-- | What an edge is, relative to the tree a traverser experiences.
-- |
-- | `ForwardEdge` is in the vocabulary because the vocabulary is the classical
-- | one, but **it cannot occur against a breadth-first tree** and `induce` will
-- | never return it. If `from` is a proper ancestor of `to` then the edge itself
-- | bounds `depth to <= depth from + 1`, while the ancestry bounds it below by
-- | the same quantity, so `to`'s parent is `from` and the edge is a `TreeEdge`.
-- | A depth-first induce would populate it; a shortest-path one cannot, and the
-- | shortcut it was meant to name shows up as a `TreeEdge` that shortens the
-- | tree instead. Retained so the classification reads the same as everyone
-- | else's and so a future DFS variant has somewhere to put its answer.
data EdgeClass
  = TreeEdge         -- ^ how you first arrive; the spine of the mental model
  | BackEdge         -- ^ to an ancestor: the way out
  | ForwardEdge      -- ^ skips levels toward a descendant: a shortcut
  | CrossEdge        -- ^ into a different subtree: an interlevel jump
  | SelfEdge         -- ^ a self-loop
  | FromUnreachable  -- ^ leaves a node the root cannot reach at all

derive instance eqEdgeClass :: Eq EdgeClass
derive instance ordEdgeClass :: Ord EdgeClass

instance showEdgeClass :: Show EdgeClass where
  show = edgeClassLabel

edgeClassLabel :: EdgeClass -> String
edgeClassLabel = case _ of
  TreeEdge -> "tree"
  BackEdge -> "back"
  ForwardEdge -> "forward"
  CrossEdge -> "cross"
  SelfEdge -> "self"
  FromUnreachable -> "unreachable"

-- | The shortest-path tree rooted at `root`, plus a verdict on every edge.
type Induced node =
  { root :: node
  , depth :: Map node Int             -- ^ fewest steps from the root
  , returnCost :: Map node Int        -- ^ fewest steps back to the root
  , parent :: Map node node
  , children :: Map node (Array node)
  , classes :: Map (Tuple node node) EdgeClass
  , unreachable :: Array node
  }

-- | Induce the tree, and classify every edge against it.
induce :: forall node. Ord node => node -> SimpleGraph node -> Induced node
induce root graph =
  { root
  , depth: forward.depth
  , returnCost: backward.depth
  , parent: forward.parent
  , children
  , classes: Map.fromFoldable (map (\e -> Tuple e (classify e)) allEdges)
  , unreachable: Array.filter (\n -> not (Map.member n forward.depth)) graph.nodes
  }
  where
  allEdges = getAllEdges graph

  forward = bfsTree root graph
  -- Reversing the edges and searching again gives, for every node, the fewest
  -- steps back to the root — which is the other half of "in by one, out by four".
  backward = bfsTree root (reverseGraph graph)

  -- Invert the parent map by walking the search's own visit order, so siblings
  -- come out in the order the search met them. Folding the parent map instead
  -- would leave sibling order at the mercy of how `Map` combines colliding
  -- values, and sibling order is exactly what a tree layout reads off this.
  children =
    Array.foldl
      ( \acc child -> case Map.lookup child forward.parent of
          Nothing -> acc
          Just par -> Map.alter (Just <<< (_ <> [ child ]) <<< fromMaybe []) par acc
      )
      Map.empty
      forward.order

  isAncestorOf ancestor node = climb node
    where
    climb n = case Map.lookup n forward.parent of
      Nothing -> false
      Just p -> p == ancestor || climb p

  classify (Tuple from to)
    | from == to = SelfEdge
    | not (Map.member from forward.depth) = FromUnreachable
    | Map.lookup to forward.parent == Just from = TreeEdge
    | isAncestorOf to from = BackEdge
    | isAncestorOf from to = ForwardEdge
    | otherwise = CrossEdge

classOf :: forall node. Ord node => Induced node -> node -> node -> Maybe EdgeClass
classOf induced from to = Map.lookup (Tuple from to) induced.classes

depthOf :: forall node. Ord node => Induced node -> node -> Maybe Int
depthOf induced node = Map.lookup node induced.depth

returnCostOf :: forall node. Ord node => Induced node -> node -> Maybe Int
returnCostOf induced node = Map.lookup node induced.returnCost

-- | Nodes that are much harder to leave than to reach.
-- |
-- | The single most reliable signature of an awful menu: in by one press, out
-- | by four. Reported worst-first, and only where the return actually costs
-- | more than the arrival.
asymmetries
  :: forall node
   . Ord node
  => Induced node
  -> Array { state :: node, inCost :: Int, outCost :: Int, excess :: Int }
asymmetries induced =
  Array.sortBy (\a b -> compare b.excess a.excess) $
    Array.mapMaybe measure (Map.toUnfoldable induced.depth :: Array (Tuple node Int))
  where
  measure (Tuple state inCost) = case Map.lookup state induced.returnCost of
    Just outCost | outCost > inCost ->
      Just { state, inCost, outCost, excess: outCost - inCost }
    _ -> Nothing

-- | How you get here from the root, as the tree tells it.
-- |
-- | The traverser's own question, and the induced tree is exactly the thing that
-- | answers it. Empty for a node the root cannot reach.
-- |
-- | An `Induced` is a search tree with extra fields, so `treePath` walks it
-- | unchanged; this name exists because the question is worth naming.
pathFromRoot :: forall node. Ord node => Induced node -> node -> Array node
pathFromRoot = treePath
