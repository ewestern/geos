{-# LANGUAGE OverloadedStrings #-}

module Data.Geometry.Geos.Serialize (
    readHex
  , writeHex
  , readWkt
) where

import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Serialize as S
import Data.Geometry.Geos.Types
import qualified Data.ByteString.Char8 as BC

readHex :: BC.ByteString -> Some Geometry
readHex bs = runGeos $ do
  r <- S.createReader
  g <- S.readHex r bs
  convertGeometryFromRaw g

writeHex :: Geometry a -> BC.ByteString
writeHex g = runGeos $ do
  w <- S.createWriter
  S.writeHex w =<< convertGeometryToRaw g

readWkt :: BC.ByteString -> Some Geometry
readWkt bs = runGeos $ do
  r <- S.createWktReader
  g <- S.readWkt r bs
  convertGeometryFromRaw g
