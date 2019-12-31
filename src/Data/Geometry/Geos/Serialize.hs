{-|
Module      : Data.Geometry.Geos.Serialize
Maintainer  : pfrance@gmail.com

Functions to read and write geometries in WKB and WKT formats.

Empty Points cannot be represented in WKB; an IllegalArgumentException will be thrown if one is written. The WKB specification does not support representing 'LinearRing', they will be written as 'LineString'

Note the WKT is not able to represent an SRID, so conversions of geometries which contain an SRID into WKT will be lossy.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Geometry.Geos.Serialize
  ( readHex
  , readLotsOfHex
  , writeHex
  , readWkt
  , writeWkt
  )
where

import           Data.Geometry.Geos.Raw.Base
import           Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Serialize
                                               as S
import qualified Data.Geometry.Geos.Raw.Geometry
                                               as R
import qualified Data.ByteString.Char8         as BC

readHex :: BC.ByteString -> Maybe (Some Geometry)
readHex bs = runGeosM $ do
  r <- S.createReader
  g <- S.readHex r bs
  convertGeometryFromRaw g

readLotsOfHex :: [BC.ByteString] -> Maybe [Some Geometry]
readLotsOfHex bs = runGeosM $ do
  r <- S.createReader
  x <- traverse (S.readHex r) bs
  traverse convertGeometryFromRaw x

writeHex :: Geometry a -> BC.ByteString
writeHex g = runGeos $ do
  w           <- S.createWriter
  r :: R.Geom <- convertGeometryToRaw g
  S.writeHex w r

readWkt :: BC.ByteString -> Maybe (Some Geometry)
readWkt bs = runGeosM $ do
  r <- S.createWktReader
  g <- S.readWkt r bs
  convertGeometryFromRaw g

writeWkt :: Geometry a -> BC.ByteString
writeWkt g = runGeos $ do
  w           <- S.createWktWriter
  r :: R.Geom <- convertGeometryToRaw g
  S.writeWkt w r
