{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module Main where
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
import Debug.Trace

import ParsingSpec
import RawGeometrySpec
import SpatialOperationsSpec

main :: IO ()
main = hspec $ do
  rawGeometrySpecs
  parsingSpecs
  spatialOpsSpecs
