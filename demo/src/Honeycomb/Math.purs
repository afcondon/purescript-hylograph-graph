-- | Math functions for Honeycomb demo
-- |
-- | Provides basic trigonometry. No stdlib package exists in the registry.
module Honeycomb.Math
  ( cos
  , sin
  , pi
  ) where

foreign import cos :: Number -> Number
foreign import sin :: Number -> Number
foreign import pi :: Number
