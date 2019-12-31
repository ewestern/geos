{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Test.Hspec

import Data.Geometry.Geos.TopologySpec
import Data.Geometry.Geos.RelatableSpec
import Data.Geometry.Geos.SerializeSpec

main :: IO ()
main = hspec $ do
  topologySpec
  relatableSpec
  serializeSpec
