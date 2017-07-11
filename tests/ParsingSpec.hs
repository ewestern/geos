{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module ParsingSpec where

import Prelude hiding (read)
import qualified Data.ByteString as BS
import Test.Hspec
import Control.Exception
import Data.Geometry.Geos.Types
import Data.Geometry.Geos.Serialize
import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.CoordSeq as RC
import qualified Data.Geometry.Geos.Raw.Serialize as RS
import qualified Data.Geometry.Geos.Raw.Geometry as R
import qualified Data.Vector as V

import SpecSampleData

parsingSpecs = describe "Tests Serialization" $ do
  it "Parses a bytestring to a linestring" $  do
    (fmap ensureLineString (readHex linestringBS)) `shouldBe` (Just linestring)
  it "Parse a multipolygon" $ do
    (fmap ensureMultiPolygon (readHex multiPolygonStringBS)) `shouldBe` (Just multiPolygon)
  it "Serializes a LineString into a bytestring" $ do
    linestringBS `shouldBe` writeHex linestring
  it "Serializes a LineString to WKT" $ do
    linestringWkt `shouldBe` (writeWkt linestring)
  it "Can parse WKT" $ do
    ensureLineString (readWkt (Just 4326) linestringWkt) `shouldBe` linestring
  it "can parse lots of things" $ do
    polygons <- (fmap ensurePolygon) <$> loadThingsFromFile "tests/sampledata/polygons.csv"
    (length polygons) `shouldBe` 98
    points <- (fmap ensurePoint) <$> loadThingsFromFile "tests/sampledata/points.csv"
    (length points) `shouldBe` 34582
  it "can read a long list" $ do
    points <- (fmap ensurePoint) <$> loadThingsFromFile "tests/sampledata/points.csv"
    (length points) `shouldBe` 34582
    -- Because we're in Australia (below the equator) all the points in this set should have a latitude < 0
    let filteredPoints = filter (\(PointGeometry (Point (Coordinate2 lat long)) _) -> long > 0) points
    -- running this test multiple times will generate a random length list
    length filteredPoints `shouldBe` 0
  it "can read a long list in one op" $ do
    points <- (fmap ensurePoint) <$> loadThingsFromFile' "tests/sampledata/points.csv"
    (length points) `shouldBe` 34582
    -- Because we're in Australia (below the equator) all the points in this set should have a latitude < 0
    let filteredPoints = filter (\(PointGeometry (Point (Coordinate2 lat long)) _) -> long > 0) points
    -- running this test multiple times will generate a random length list
    length filteredPoints `shouldBe` 0
  it "handles nonesense properly" $ do
    let bs = "2D50491A5DC024275C1ED5DE4040" :: BS.ByteString
    Nothing `shouldBe` (fmap ensurePoint $ readHex bs)


searchPoints :: [Geometry Point] -> Geometry Polygon -> [Geometry Point]
searchPoints points polygon = filter f points
  where f point = contains polygon point
