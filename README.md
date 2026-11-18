# purescript-hylograph-graph

Graph algorithms and data structures for PureScript, designed for visualization.

Live interactive demo can be seen as part of the [suite of Hylograph library demos](https://afcondon.github.io/purescript-hylograph-demos/). Click the screenshot below for **The Honeycomb** puzzle. (The puzzle carries its own breadth-first search and does not call this library; the [Graph Decomposition Explorer](https://afcondon.github.io/purescript-hylograph-demos/graph-decomposition/) is the demo that does.)

[![Honeycomb Puzzle Demo](https://afcondon.github.io/purescript-hylograph-demos/thumbnails/graph.jpg)](https://afcondon.github.io/purescript-hylograph-demos/graph/)

## Overview

This package provides graph algorithms with optional tracing for visualization, consolidating graph-related code from the Hylograph ecosystem.

## Features

### Graph Algorithms
- **Pathfinding**: Dijkstra shortest path, BFS/DFS. There is no A* (no heuristic search); `findPath` is an alias for `shortestPath`, kept for an older demo.
- **Analysis**: Reachability, transitive reduction, layer computation, connected and strongly connected components, PageRank
- **Decomposition**: Articulation points, bridges, biconnected components, block-cut tree
- **DAG operations**: Topological sort, cycle detection

### Tree Utilities
- Rose tree helpers (wrapping `tree-rose`)
- Tree manipulation functions
- DAGTree for "mostly hierarchical" graphs

### Visualization Support
- Optional algorithm tracing for step-by-step animation
- Position helpers for layout

## Installation

```bash
spago install hylograph-graph
```

## Usage

### Basic Pathfinding

```purescript
import Data.Graph.Pathfinding (PathResult, shortestPath)
import Data.Graph.Types (Graph, NodeId(..), mkGraph)

myGraph :: Graph
myGraph = mkGraph
  [ NodeId "a", NodeId "b", NodeId "c" ]
  [ { from: NodeId "a", to: NodeId "b", weight: 1.0 }
  , { from: NodeId "b", to: NodeId "c", weight: 2.0 }
  ]

result :: PathResult
result = shortestPath (NodeId "a") (NodeId "c") myGraph
```

### With Tracing (for visualization)

```purescript
import Data.Graph.Pathfinding.Traced (TracedResult, shortestPathTraced)

traced :: TracedResult PathResult
traced = shortestPathTraced (NodeId "a") (NodeId "c") myGraph
-- traced.steps is the sequence of visits and edge explorations to animate
```

## Modules

- `Data.Graph.Pathfinding` - Dijkstra, BFS, DFS
- `Data.Graph.Pathfinding.Traced` - The same, with step-by-step tracing
- `Data.Graph.Algorithms` - Graph analysis (reachability, reduction, components, centrality)
- `Data.Graph.Decomposition` - Articulation points, bridges, biconnected components
- `Data.Graph.Layout` - Position helpers for graph visualization
- `Data.Graph.Types` - Core graph types
- `Hylograph.Data.DAGTree` - Hybrid DAG/tree structure

## Part of Hylograph

This package is part of the Hylograph visualization ecosystem:

- **hylograph-graph** - Graph algorithms (this package)
- **hylograph-selection** - HATS trees and DOM rendering
- **hylograph-simulation** - Force-directed graph simulation
- **hylograph-layout** - Layout algorithms

## License

MIT
