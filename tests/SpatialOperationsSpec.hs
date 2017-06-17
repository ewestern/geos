{-# LANGUAGE OverloadedStrings #-}

module SpatialOperationsSpec where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Geometry.Geos.Types
import Data.Geometry.Geos.Geometry

import SpecSampleData

spatialOpsSpecs = describe "Tests Contains" $ do
  it "Does simple polygon to point comparison" $ do
    let polygon = makePolygonGeo [(0,0), (0,1), (1,1), (1,0), (0,0)]
        point   = makePointGeo (0.5, 0.5)
    (contains polygon point) `shouldBe` True
