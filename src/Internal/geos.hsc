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
data GEOSCoordSequence
data GEOSSTRtree
data GEOSBufferParams

type GEOSMessageHandler = CString -> IO ()
-- read/write
data GEOSWKBWriter
data GEOSWKBReader

{-instance Storable GEOSContextHandler where-}

foreign import ccall unsafe 
  "GEOS/geos_c.h initGEOS_r"
   geos_initGEOS_r :: FunPtr GEOSMessageHandler -> FunPtr GEOSMessageHandler -> IO (Ptr GEOSContextHandle)

foreign import ccall unsafe
  "GEOS/geos_c.h finishGEOS_r"
  geos_finishGEOS_r :: Ptr GEOSContextHandle -> IO ()

 -- Coord Sequence  -- return 0 on exception
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_create_r"
  geos_CoordSeqCreate :: Ptr GEOSContextHandle -> CUInt -> CUInt -> IO (Ptr GEOSCoordSequence)  

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_destroy_r" 
  geos_CoordSeqDestroy :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> IO ()

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setX_r"
  geos_CoordSeqSetX :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setY_r"
  geos_CoordSeqSetY :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setZ_r"
  geos_CoordSeqSetZ :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setOrdinate_r"
  geos_CoordSeqSetOrdinate :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> CUInt -> CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getX_r"
  geos_CoordSeqGetX :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getY_r"
  geos_CoordSeqGetY :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getZ_r"
  geos_CoordSeqGetZ :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getOrdinate_r"
  geos_CoordSeqGetOrdinate :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> CUInt -> CUInt -> Ptr CDouble -> IO CInt

--- Geometry Constructors

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createPoint_r"
  geos_GeomCreatePoint :: Ptr GEOSContextHandle -> Ptr GEOSCoordSequence -> IO (Ptr GEOSGeometry)


{-foreign import ccall unsafe-}
  {-"GEOS/geos_c.h"-}
  
---  Readers / Writers
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_create_r"
  geos_WKBReaderCreate :: Ptr GEOSContextHandle -> IO (Ptr GEOSWKBReader) 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_destroy_r"
  geos_WKBReaderDestroy :: Ptr GEOSContextHandle -> Ptr GEOSWKBReader -> IO ()

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_read_r"
  geos_WKBReaderRead :: Ptr GEOSContextHandle -> Ptr GEOSWKBReader -> CString  -> CSize -> Ptr GEOSGeometry 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_readHEX_r"
  geos_WKBReaderReadHex :: Ptr GEOSContextHandle -> Ptr GEOSWKBReader -> CString -> CSize -> Ptr GEOSGeometry 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBWriter_create_r"
  geos_WKBWriterCreate :: Ptr GEOSContextHandle -> IO (Ptr GEOSWKBWriter) 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBWriter_destroy_r"
  geos_WKBWriterDestroy :: Ptr GEOSContextHandle -> Ptr GEOSWKBWriter -> IO ()

  -- caller owns results
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBWriter_write_r"
  geos_WKBWriterWrite :: Ptr GEOSContextHandle -> Ptr GEOSWKBWriter -> Ptr GEOSGeometry -> Ptr CSize -> IO CString

  -- caller owns results
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBWriter_writeHEX_r"
  geos_WKBWriterWriteHex :: Ptr GEOSContextHandle -> Ptr GEOSWKBWriter -> Ptr GEOSGeometry -> Ptr CSize -> IO CString
  --1085 finish writer methods




