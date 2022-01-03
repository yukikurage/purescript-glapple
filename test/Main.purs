module Test.Main where

import Prelude

import Effect (Effect)
import Test.Maker as Maker

main :: Effect Unit
main = do
  Maker.test
