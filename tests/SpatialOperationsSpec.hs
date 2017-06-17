{-# LANGUAGE OverloadedStrings #-}

module SpatialOperationsSpec where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Geometry.Geos.Types
import Data.Geometry.Geos.Geometry

import SpecSampleData

spatialOpsSpecs = describe "Tests Contains" $ do
  it "Does polygon to point comparison" $ do
    let polygon         = makePolygonGeo [[(0,0), (0,1), (1,1), (1,0), (0,0)]]
        polygonWithHole = makePolygonGeo [ [(0,0), (0,1), (1,1), (1,0), (0,0)], [(0.1,0.1),(0.1,0.9),(0.9,0.1),(0.1,0.1)] ]
        pointIn         = makePointGeo (0.6, 0.6)
        pointOut        = makePointGeo (1.5, 0.5)
    (contains polygon pointIn) `shouldBe` True
    (contains polygon pointOut) `shouldBe` False
    (contains polygonWithHole pointIn) `shouldBe` True
    (contains polygonWithHole pointOut) `shouldBe` False
  it "Does simple polygon to polygon comparison" $ do
    let polygonBig   = makePolygonGeo [ [(0,0), (0,2), (2,2), (2,0), (0,0)] ]
        polygonSmall = makePolygonGeo [ [(0,0), (0,1), (1,1), (1,0), (0,0)] ]
        polygonIntersect = makePolygonGeo [ [(0,0), (0,2.5), (1,1), (1,0), (0,0)] ]
    (contains polygonBig polygonSmall) `shouldBe` True
    (contains polygonBig polygonBig) `shouldBe` True
    (contains polygonBig polygonIntersect) `shouldBe` False

  it "Does multi polygon to point comparison" $ do
    -- pendingWith "Causes double free on memory"
    let polygonBig   = [ [(0,0), (0,2), (2,2), (2,0), (0,0)]]
        polygonSmall = [ [(3,0), (3,1), (4,1), (4,0), (3,0)]]
        multiPoly    = makeMultiPolygonGeo [polygonBig, polygonSmall]
        pointIn      = makePointGeo (0.5, 0.5)
        pointOut     = makePointGeo (2.5, 0.5)
    (contains multiPoly pointIn) `shouldBe` True
    (contains multiPoly pointOut) `shouldBe` False
