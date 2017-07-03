{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module ParsingSpec where

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
    let lsg :: Geometry LineString = withSomeGeometry (readHex linestringBS) $ \case
                ls@(LineStringGeometry _ _) -> ls
                _ -> error "asda"
    lsg `shouldBe` linestring
  it "Parse a multipolygon" $ do
    let multig :: Geometry MultiPolygon = withSomeGeometry (readHex multiPolygonStringBS) $ \case
                g@(MultiPolygonGeometry _ _) -> g
                _ -> error "asda"
    multig `shouldBe` multiPolygon
  it "Serializes a LineString into a bytestring" $ do
    linestringBS `shouldBe` writeHex linestring
  it "Serializes a LineString to WKT" $ do
    linestringWkt `shouldBe` (writeWkt linestring)
  it "Can parse WKT" $ do
    let lsg :: Geometry LineString = withSomeGeometry (readWkt (Just 4326) linestringWkt) $ \case
                ls@(LineStringGeometry _ _) -> ls
                _ -> error "asda"
    lsg `shouldBe` linestring
  it "can parse lots of things" $ do
    polygons <- (fmap ensurePolygon) <$> loadThingsFromFile "tests/sampledata/polygons.csv"
    (length polygons) `shouldBe` 98
    points <- (fmap ensurePoint) <$> loadThingsFromFile "tests/sampledata/points.csv"
    (length points) `shouldBe` 34582
    let matchingPoints = searchPoints points <$> polygons
    (length matchingPoints) `shouldBe` 98
    mapM_ (print . length) matchingPoints
    1 `shouldBe` 1
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




ensurePoint :: Some Geometry -> Geometry Point
ensurePoint g = withSomeGeometry g $ \g' -> case g' of
  PointGeometry _ _ -> g'
  _ -> error "This geometry was expected to be a Point"

ensurePolygon :: Some Geometry -> Geometry Polygon
ensurePolygon g = withSomeGeometry g $ \g' -> case g' of
  PolygonGeometry _ _  -> g'
  _ -> error "This geometry was expected to be a Polygon"

searchPoints :: [Geometry Point] -> Geometry Polygon -> [Geometry Point]
searchPoints points polygon = filter f points
  where f point = contains polygon point
