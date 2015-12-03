{-# LANGUAGE OverloadedStrings #-}

module GEOS.Serialize (
    readHex
  , writeHex
) where

import GEOS.Raw.Base
import GEOS.Geometry
import qualified GEOS.Raw.Serialize as S
import GEOS.Types
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
