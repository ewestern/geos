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
import Data.Maybe (fromJust)

import SpecSampleData

parsingSpecs = describe "Tests Serialization" $ do
  it "Parses a bytestring to a linestring" $  do
    ensureLineString <$> readHex linestringBS `shouldBe` Just linestring
  it "Parse a multipolygon" $ do
    ensureMultiPolygon <$> readHex multiPolygonStringBS `shouldBe` Just multiPolygon
  it "Serializes a LineString into a bytestring" $ do
    linestringBS `shouldBe` writeHex linestring
  it "Serializes a LineString to WKT" $ do
    linestringWkt `shouldBe` (writeWkt linestring)
  it "Can parse WKT" $ do
    ensureLineString <$> readWkt (Just 4326) linestringWkt `shouldBe` Just linestring
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

  it "can write/read with zero loss of information" $ do
    let bs = "0105000020C46E000001000000010200000004000000EE2DE25E859A20410829F224364B5A41BD757FB6679A204115C6D8E9304B5A4177111862469A2041A03F04E32C4B5A41BC1D3ADD459A2041793C8CD92C4B5A41" :: BS.ByteString
        srid = Just 28356
        points = [[(544066.685319362, 6892760.57728029),
                   (544051.856441192, 6892739.65385582),
                   (544035.191589876, 6892723.54713431),
                   (544034.932084016, 6892723.39918434)]]
        expectedGeo = MultiLineStringGeometry (makeMultiLineString points) srid
        parsedGeo = fromJust (fmap ensureMultiLineString $ readHex bs)
    parsedGeo `shouldBe` expectedGeo
    let encodedHex = writeHex expectedGeo
    encodedHex `shouldBe` bs





searchPoints :: [Geometry Point] -> Geometry Polygon -> [Geometry Point]
searchPoints points polygon = filter f points
  where f point = contains polygon point
