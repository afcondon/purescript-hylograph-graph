# purescript-hylograph-graph

[![Honeycomb Puzzle Demo](../../site/lib-graph/public/demo.jpeg)](/honeycomb/)

Graph algorithms and data structures for PureScript, designed for visualization.

## Overview

This package provides graph algorithms with optional tracing for visualization, consolidating graph-related code from the Hylograph ecosystem.

## Features

### Graph Algorithms
- **Pathfinding**: A*, Dijkstra, BFS/DFS
- **Analysis**: Reachability, transitive reduction, layer computation
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
import Data.Graph.Pathfinding (findPath)

result = findPath startNode endNode myGraph
```

### With Tracing (for visualization)

```purescript
import Data.Graph.Pathfinding.Traced (findPathTraced)

{ result, steps, explored } = findPathTraced startNode endNode myGraph
-- Use `steps` to animate the algorithm
```

## Modules

- `Data.Graph.Pathfinding` - Basic pathfinding algorithms
- `Data.Graph.Pathfinding.Traced` - Pathfinding with step-by-step tracing
- `Data.Graph.Algorithms` - Graph analysis (reachability, reduction)
- `Data.Graph.Layout` - Position helpers for graph visualization
- `Data.Graph.Types` - Core graph types
- `Hylograph.Data.DAGTree` - Hybrid DAG/tree structure

## Part of Hylograph

This package is part of the Hylograph visualization ecosystem:

- **hylograph-graph** - Graph algorithms (this package)
- **hylograph-selection** - Type-safe D3 selection and rendering
- **hylograph-simulation** - Force-directed graph simulation
- **hylograph-layout** - Layout algorithms

## License

MIT
