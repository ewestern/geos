{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module GEOS.Internal where

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

data GEOSContextHandle_HS
type GEOSContextHandle_t = Ptr GEOSContextHandle_HS
data GEOSGeometry
data GEOSCoordSequence
data GEOSSTRtree
data GEOSBufferParams
type GEOSMessageHandler = CString -> IO ()

-- read/write
data GEOSWKBWriter
data GEOSWKBReader


foreign import ccall 
  "wrapper"
  mkMessageHandler :: GEOSMessageHandler -> IO (FunPtr GEOSMessageHandler)  

foreign import ccall unsafe 
  "GEOS/geos_c.h initGEOS_r"
   geos_initGEOS :: FunPtr GEOSMessageHandler -> FunPtr GEOSMessageHandler -> IO GEOSContextHandle_t

foreign import ccall unsafe
  "GEOS/geos_c.h &finishGEOS_r"
  geos_finishGEOS :: FunPtr (GEOSContextHandle_t -> IO ())


-- Info

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetSRID_r"
  geos_GetSRID :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSSetSRID_r"
  geos_SetSRID :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CInt -> IO ()

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_getCoordSeq_r"
  geos_GetCoordSeq :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSCoordSequence)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeomTypeId_r"
  geos_GeomTypeId :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeomType_r"
  geos_GeomType :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CChar


 -- Coord Sequence  -- return 0 on exception
 --
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_create_r"
  geos_CoordSeqCreate :: GEOSContextHandle_t -> CUInt -> CUInt -> IO (Ptr GEOSCoordSequence)  


foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_clone_r"
  geos_CoordSeqClone :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSCoordSequence )

foreign import ccall unsafe
  "GEOS/geos_c.h &GEOSCoordSeq_destroy_r" 
  geos_CoordSeqDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO ())

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setX_r"
  geos_CoordSeqSetX :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setY_r"
  geos_CoordSeqSetY :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setZ_r"
  geos_CoordSeqSetZ :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_setOrdinate_r"
  geos_CoordSeqSetOrdinate :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CUInt -> CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getX_r"
  geos_CoordSeqGetX :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getY_r"
  geos_CoordSeqGetY :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getZ_r"
  geos_CoordSeqGetZ :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getOrdinate_r"
  geos_CoordSeqGetOrdinate :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getSize_r"
  geos_CoordSeqGetSize :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> Ptr CUInt -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetNumCoordinates_r"
  geos_getNumCoordinates :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getDimensions_r"
  geos_CoordSeqGetDimensions :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> Ptr CUInt -> IO CInt

--- Geometry Constructors

foreign import ccall unsafe
  "GEOS/geos_c.h &GEOSGeom_destroy_r"
  geos_GeomDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSGeometry -> IO ())

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createPoint_r"
  geos_GeomCreatePoint :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createEmptyPoint_r"
  geos_GeomCreateEmptyPoint :: GEOSContextHandle_t -> IO (Ptr GEOSGeometry)
  
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createLinearRing_r"
  geos_GeomCreateLinearRing ::  GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createLineString_r"
  geos_GeomCreateLineString ::  GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createEmptyLineString_r"
  geos_GeomCreateEmptyLineString :: GEOSContextHandle_t -> IO (Ptr GEOSGeometry)

--- Topology
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSEnvelope_r"
  geos_Envelope :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry) 
  
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSIntersection_r"
  geos_Intersection :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSIntersection_r"
  geos_ConvexHull :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSDifference_r"
  geos_Difference :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSSymDifference_r"
  geos_SymDifference :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSBoundary_r"
  geos_Boundary :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSUnion_r"
  geos_Union :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)


foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetCentroid_r"
  geos_GetCentroid :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

-----
--Binary Predicates.
-----

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSDisjoint_r"
  geos_Disjoint :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSTouches_r"
  geos_Touches :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

--- 650
  
--- prepared Geometries
--688
--

---  Readers / Writers
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_create_r"
  geos_WKBReaderCreate :: GEOSContextHandle_t -> IO (Ptr GEOSWKBReader) 

foreign import ccall unsafe
  "GEOS/geos_c.h &GEOSWKBReader_destroy_r"
  geos_WKBReaderDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSWKBReader -> IO ())

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_read_r"
  geos_WKBReaderRead :: GEOSContextHandle_t -> Ptr GEOSWKBReader -> CString  -> CSize -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBReader_readHEX_r"
  geos_WKBReaderReadHex :: GEOSContextHandle_t -> Ptr GEOSWKBReader -> CString -> CSize -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBWriter_create_r"
  geos_WKBWriterCreate :: GEOSContextHandle_t -> IO (Ptr GEOSWKBWriter) 

foreign import ccall unsafe
  "GEOS/geos_c.h &GEOSWKBWriter_destroy_r"
  geos_WKBWriterDestroy :: FunPtr ( GEOSContextHandle_t -> Ptr GEOSWKBWriter -> IO ())

  -- caller owns results
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBWriter_write_r"
  geos_WKBWriterWrite :: GEOSContextHandle_t -> Ptr GEOSWKBWriter -> Ptr GEOSGeometry -> Ptr CSize -> IO CString

  -- caller owns results
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWKBWriter_writeHEX_r"
  geos_WKBWriterWriteHex :: GEOSContextHandle_t -> Ptr GEOSWKBWriter -> Ptr GEOSGeometry -> Ptr CSize -> IO CString
--1085 finish writer methods
