-- | Main entry point for Honeycomb Puzzle
module Honeycomb.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Honeycomb.Component as Honeycomb

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Honeycomb.component unit body
