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


