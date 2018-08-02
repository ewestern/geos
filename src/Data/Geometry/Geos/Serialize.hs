{-|
Module      : Data.Geometry.Geos.Serialize
Maintainer  : pfrance@gmail.com

Functions to read and write geometries in WKB and WKT formats.

The WKB format is specified in the OGC Simple Features for SQL specification. This implementation supports the extended WKB standard for representing 3-dimensional coordinates. The presence of 3D coordinates is signified by setting the high bit of the wkbType word.
Empty Points cannot be represented in WKB; an IllegalArgumentException will be thrown if one is written. The WKB specification does not support representing 'LinearRing', they will be written as 'LineString'
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Geometry.Geos.Serialize (
    readHex
  , readLotsOfHex
  , writeHex
  , readWkt
  , writeWkt
) where

import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Serialize as S
import qualified Data.Geometry.Geos.Raw.Geometry as R
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (catMaybes)

readHex :: BC.ByteString -> Maybe (Some Geometry)
readHex bs = runGeos $ do
  r <- S.createReader
  g <- S.readHex r bs
  case g of
    Just g' -> Just <$> convertGeometryFromRaw g'
    Nothing -> return Nothing

readLotsOfHex :: [BC.ByteString] -> [Some Geometry]
readLotsOfHex bs = runGeos $ do
  r <- S.createReader
  x <- traverse (S.readHex r) bs
  traverse convertGeometryFromRaw $ catMaybes x

writeHex :: Geometry a -> BC.ByteString
writeHex g = runGeos $ do
  w <- S.createWriter
  r :: R.Geom <- convertGeometryToRaw g
  S.writeHex w r

-- Read a string of WKT, optionally adding an SRID to the resulting geometry.
readWkt :: Maybe Int -> BC.ByteString -> Maybe (Some Geometry)
readWkt srid bs = runGeos $ do
  r <- S.createWktReader
  g <- S.readWkt r bs
  case g of
    Just g' -> do
      g'' <- R.setSRID srid g'
      Just <$> convertGeometryFromRaw g''
    Nothing -> return Nothing

writeWkt :: Geometry a -> BC.ByteString
writeWkt g = runGeos $ do
  w <- S.createWktWriter
  r :: R.Geom <- convertGeometryToRaw g
  S.writeWkt w r
