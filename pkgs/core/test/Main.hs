module Main where

import "hspec" Test.Hspec (hspec)

import Effex.Core.ModuleSpec qualified as ModuleSpec

main :: IO ()
main = hspec do
  ModuleSpec.spec
