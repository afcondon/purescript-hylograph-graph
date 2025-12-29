-- | Layout types for graph and tree visualization
module Data.Graph.Layout
  ( TreeLayout(..)
  ) where

import Prelude

-- | Tree layout orientation
data TreeLayout = Radial | Horizontal | Vertical

derive instance eqTreeLayout :: Eq TreeLayout
derive instance ordTreeLayout :: Ord TreeLayout

instance showTreeLayout :: Show TreeLayout where
  show Radial = "Radial"
  show Horizontal = "Horizontal"
  show Vertical = "Vertical"
