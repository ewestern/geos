{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module RawGeometrySpec where

import qualified Data.ByteString as BS
import Test.Hspec
import Control.Exception
import Data.Geometry.Geos.Relatable
import Data.Geometry.Geos.Serialize
import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.CoordSeq as RC
import qualified Data.Geometry.Geos.Raw.Serialize as RS
import qualified Data.Geometry.Geos.Raw.Geometry as R
import qualified Data.Vector as V
import Data.Maybe (fromJust)

import SpecSampleData

rawGeometrySpecs = describe "raw geometry" $ do

  it "Creates a Coordinate Sequence" $  do
    let (size, dim) = runGeos $ do
          cs :: RC.CoordSeq <- RC.createEmptyCoordinateSequence 2 2
          size <-  RC.getCoordinateSequenceSize cs
          dim <-  RC.getCoordinateSequenceDimensions cs
          return (size, dim)
    size `shouldBe` (2 :: Int)
    dim `shouldBe` (2 :: Int)
  it "Sets a Coordinate Sequence" $ do
    let (d1, d2) = runGeos $ do
          c :: RC.CoordSeq <- RC.createEmptyCoordinateSequence 2 2
          RC.setCoordinateSequenceX c 0 5.0
          RC.setCoordinateSequenceY c 0 10.0
          d1 <- RC.getCoordinateSequenceX c 0
          d2 <- RC.getCoordinateSequenceY c 0
          return (d1, d2)
    d1 `shouldBe` (5.0 :: Double)
    d2 `shouldBe` (10.0 :: Double)

  it "Creates a LineString " $ do
    let tid = runGeos $ do
          cs :: RC.CoordSeqConst <- RC.createEmptyCoordinateSequence 2 2
          ls :: R.Geom <- R.createLineString cs
          R.getTypeId ls
    tid `shouldBe` R.LineStringTypeId
  it "Creates a Geometry" $ do
    pending

  it "Converts a LineString" $ do
    let (srid, tid, tn) = runGeos $ do
          l :: R.Geom <- convertGeometryToRaw linestring
          t <- R.getTypeId l
          s <- R.getSRID l
          tn <- R.getTypeName l
          return (s, t, tn)
    tid `shouldBe` R.LineStringTypeId
    srid `shouldBe` (Just 4326)
    tn `shouldBe` "LineString"
  it "Converts a Polygon" $ do
    let t = runGeos $ do
          rp :: R.Geom <-  convertGeometryToRaw $ PolygonGeometry polygon1 Nothing
          R.getTypeId rp
    t `shouldBe` R.PolygonTypeId
  it "Converts a MultiPolygon from Raw" $ do
    let (x,y) = runGeos $ do
          r <- RS.createReader
          mpr <- RS.readHex r multiPolygonStringBS
          MultiPolygon vps <- convertMultiPolygonFromRaw $ fromJust mpr
          let (Polygon vlr) = vps V.! 0
              (LinearRing vc) = vlr V.! 0
              (Coordinate2 x y) = vc V.! 0
          return (x, y)

    x `shouldBe` 153.160525
    y `shouldBe` -27.377412
  it "Tests disjoint" $ do
     (disjoint (PolygonGeometry polygon1 Nothing) (PolygonGeometry polygon2 Nothing)) `shouldBe` False
  it "Tests intersects" $ do
     intersects (PolygonGeometry polygon1 Nothing) (PolygonGeometry polygon2 Nothing) `shouldBe` True
  it "Tests raw parsing" $ do
      let (x,y) = runGeos $ do
              r <- RS.createReader
              g <- RS.readHex r multiPolygonStringBS
              gi :: R.GeomConst <- R.getGeometryN (fromJust g) 0
              ir :: R.GeomConst <- R.getExteriorRing gi
              cs <- R.getCoordinateSequence ir
              x <- RC.getCoordinateSequenceX cs 0
              y <- RC.getCoordinateSequenceY cs 0
              return (x,y)
      x `shouldBe` 153.160525
      y `shouldBe` -27.377412
  it "Tests Distance" $ do
    let a = makePointGeo (0,0)
        b = makePointGeo (0,2)
    let d = distance a b
        hd = hausdorffDistance a b
    d   `shouldBe` 2.0
    hd  `shouldBe` 2.0
