# purescript-psd3-graph

Graph algorithms and data structures for PureScript, designed for visualization.

## Overview

This package provides graph algorithms with optional tracing for visualization, consolidating graph-related code from the PSD3 ecosystem.

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
spago install psd3-graph
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

## Part of PSD3

This package is part of the PSD3 (PureScript D3) family:

- **psd3-graph** - Graph algorithms (this package)
- **psd3-selection** - Type-safe D3 selection and rendering
- **psd3-simulation** - Force-directed graph simulation

## License

MIT
