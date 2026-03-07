module Decomposition.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Decomposition.Component as Decomposition

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Decomposition.component unit body
