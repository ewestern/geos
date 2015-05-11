{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Geos.Internal where

import Foreign
import Foreign.C
import Foreign.C.String

#include <GEOS/geos_c.h>

newtype GEOSGeomType = GEOSGeomType { unGEOSGeomType :: CInt }
    deriving (Eq,Show)

#{ enum GEOSGeomType, GEOSGeomType,
    geos_point = GEOS_POINT
  , geos_lineString = GEOS_LINESTRING
  , geos_polygon = GEOS_POLYGON
  , geos_multiPoint = GEOS_MULTIPOINT
  , geos_multiLineString = GEOS_MULTIPOINT
  , geos_multiPolygon = GEOS_MULTIPOLYGON
  , geos_geometryCollection = GEOS_GEOMETRYCOLLECTION
}

newtype GEOSByteOrder = GEOSByteOrder { unGEOSByteOrder :: CInt}
  deriving (Eq, Show)

#{ enum GEOSByteOrder, GEOSByteOrder, 
    geos_bigEndian = 0
  , geos_littleEndian = 1 
}
data GEOSContextHandle
data GEOSGeometry
data GEOSCoordinateSequence
data GEOSSTRtree
data GEOSBufferParams

type GEOSMessageHandler = CString -> ()
-- read/write
data GEOSWKBWriter
data GEOSWKBReader

{-instance Storable GEOSContextHandler where-}

foreign import ccall unsafe 
  "GEOS/geos_c.h initGEOS_r"
   geos_initGEOS_r :: FunPtr GEOSMessageHandler -> FunPtr GEOSMessageHandler -> IO (Ptr GEOSContextHandle)

foreign import ccall unsafe
  "GEOS/geos_c.h finishGEOS_r"
  geos_finishGEOS_r :: GEOSContextHandle -> IO ()

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_create_r"
  geos_coordSeqCreate :: GEOSContextHandle -> CUInt -> CUInt -> IO (Ptr GEOSCoordSequence)  

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_destroy_r" 
  geos_coordSeqDestroy :: GEOSContextHandle -> (Ptr GEOSCoordSequence) -> IO ()

--- 
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_create_r"
  geos_WKBReaderCreate :: GEOSContextHandle -> IO (GEOSWKBReader) 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_destroy_r"
  geos_WKBReaderDestroy :: GEOSContextHandle -> (Ptr GEOSWKBReader) -> IO ()

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_read_r"
  geos_WKBReaderRead :: GEOSContextHandle -> (Ptr GEOSWKBReader) -> CString  -> CSize -> GEOSGeometry 

{-foreign import ccall unsafe-}
  {-"GEOS/geos_c.h GEOSWKBReader_readHEX_r"-}
  {-geos_WKBReaderReadHex :: -}
