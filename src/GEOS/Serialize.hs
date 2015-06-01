{-# LANGUAGE OverloadedStrings #-}

module GEOS.Serialize (
    readHex
  , writeHex
) where

import GEOS.Raw.Base
import qualified GEOS.Raw.Geometry as R
import GEOS.Geometry
import qualified GEOS.Raw.Serialize as S
import GEOS.Types

import qualified Data.ByteString.Char8 as BC

readHex :: BC.ByteString -> Geometry
readHex bs = 
  let h = initializeGEOS putStrLn error
      r = S.createReader h 
      g = S.readHex h r bs 
  in convertGeometryFromRaw h g

writeHex :: Geometry -> BC.ByteString
writeHex g = 
  let h = initializeGEOS putStrLn error
      w = S.createWriter h
  in S.writeHex h w $ convertGeometryToRaw h g 
