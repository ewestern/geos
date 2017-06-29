{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Geometry.Geos.Serialize (
    readHex
  , writeHex
  , readWkt
  , writeWkt
) where

import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Serialize as S
import qualified Data.Geometry.Geos.Raw.Geometry as R
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
  r :: R.Geom <- convertGeometryToRaw g
  S.writeHex w r

readWkt :: Maybe Int -> BC.ByteString -> Some Geometry
readWkt srid bs = runGeos $ do
  r <- S.createWktReader
  g <- S.readWkt r bs
  g' <- R.setSRID srid g
  convertGeometryFromRaw g'

writeWkt :: Geometry a -> BC.ByteString
writeWkt g = runGeos $ do
  w <- S.createWktWriter
  r :: R.Geom <- convertGeometryToRaw g
  S.writeWkt w r
