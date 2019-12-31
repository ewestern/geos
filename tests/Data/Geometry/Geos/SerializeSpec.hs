{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.Geos.SerializeSpec
  ( 
    serializeSpec
  )
where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import           Data.Maybe                     ( mapMaybe
                                                , catMaybes
                                                )

import           Test.Hspec
import           Helpers
import           Data.Geometry.Geos.Serialize
import           Data.Geometry.Geos.Geometry
import Debug.Trace

multiLineStringWKB = "0105000020C46E000001000000010200000004000000EE2DE25E859A20410829F224364B5A41BD757FB6679A204115C6D8E9304B5A4177111862469A2041A03F04E32C4B5A41BC1D3ADD459A2041793C8CD92C4B5A41" :: BS.ByteString

multiLineStringWKT = "MULTILINESTRING ((544066.6853193619754165 6892760.5772802904248238, 544051.8564411919796839 6892739.6538558201864362, 544035.1915898759616539 6892723.5471343100070953, 544034.9320840160362422 6892723.3991843396797776))" :: BS.ByteString

multiLineString1 = MultiLineStringGeometry (fromRight $ makeMultiLineString  [[(544066.685319362, 6892760.57728029), (544051.856441192, 6892739.65385582), (544035.191589876, 6892723.54713431), (544034.932084016, 6892723.39918434)]]) (Just 28356)

multiLineStringNoSRID = MultiLineStringGeometry (fromRight $ makeMultiLineString  [[(544066.685319362, 6892760.57728029), (544051.856441192, 6892739.65385582), (544035.191589876, 6892723.54713431), (544034.932084016, 6892723.39918434)]]) Nothing


--loadThingsFromFile :: FilePath -> IO [Some Geometry]
--loadThingsFromFile fp = do
--  rows <- BS8.readFile fp
--  return $ catMaybes $ readHex <$> (BS8.lines rows)


serializeSpec = describe "Serialize" $ do
  describe "readHex" $ do
    it "should convert a WKB bytestring to a geometry" $ (readHex multiLineStringWKB >>= ensureMultiLineString)  `shouldBe` Just multiLineString1
    it "should read a file with thousands of WKB points" $ do
      rows <- BS8.readFile "tests/sampledata/points.csv"
      let geoms = catMaybes $ readHex <$> BS8.lines rows
      geoms `shouldSatisfy` ((34582 ==) . length . mapMaybe ensurePoint)
    it "should read a file with dozens of WKB polygons" $ do
      rows <- BS8.readFile "tests/sampledata/polygons.csv"
      let geoms = catMaybes $ readHex <$> BS8.lines rows
      geoms `shouldSatisfy` ((98 ==) . length . mapMaybe ensurePolygon)
  describe "writeHex" $ 
    it "should convert a geometry to a WKB bytestring" $ writeHex multiLineString1 `shouldBe` multiLineStringWKB 

  describe "writeWkt" $ 
    it "should convert a geometry to WKT string" $ writeWkt multiLineString1  `shouldBe` multiLineStringWKT
  describe "readWkt" $ 
    it "should convert a WKT string to a geometry" $ readWkt multiLineStringWKT  `shouldBe` (Just $ Some multiLineStringNoSRID)

