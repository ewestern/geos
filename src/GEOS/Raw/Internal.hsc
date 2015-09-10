{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module GEOS.Raw.Internal where

import Foreign
import Foreign.C

#include <geos_c.h>
#include "noticehandlers.h"

newtype GEOSGeomType = GEOSGeomType { unGEOSGeomType :: CInt }
    deriving (Eq,Show)

#{ enum GEOSGeomType, GEOSGeomType,
    pointId = GEOS_POINT
  , lineStringId = GEOS_LINESTRING
  , polygonId = GEOS_POLYGON
  , multiPointId = GEOS_MULTIPOINT
  , multiLineStringId = GEOS_MULTIPOINT
  , multiPolygonId = GEOS_MULTIPOLYGON
  , geometryCollectionId = GEOS_GEOMETRYCOLLECTION
}

newtype GEOSByteOrder = GEOSByteOrder { unGEOSByteOrder :: CInt}
  deriving (Eq, Show)

#{ enum GEOSByteOrder, GEOSByteOrder, 
    bigEndian = 0
  , littleEndian = 1 
}

data GEOSContextHandle_HS
type GEOSContextHandle_t = Ptr GEOSContextHandle_HS
data GEOSGeometry
data GEOSCoordSequence
data GEOSSTRtree
data GEOSBufferParams
data GEOSPreparedGeometry
type GEOSMessageHandler = FunPtr (CString -> (Ptr ()) -> IO ())
type GEOSMessageHandler_r = FunPtr (CString -> (Ptr ()) -> IO ())

-- read/write
data GEOSWKBWriter
data GEOSWKBReader


foreign import ccall unsafe 
  "notice_handlers.h init_GEOS"
   geos_init :: IO GEOSContextHandle_t

foreign import ccall unsafe
  "geos_c.h &finishGEOS_r"
  geos_finish :: FunPtr (GEOSContextHandle_t -> IO ())

foreign import ccall unsafe
  "geos_c.h GEOSContext_setNoticeHandler_r"
  geos_setNoticeHandler :: GEOSContextHandle_t -> GEOSMessageHandler -> IO GEOSMessageHandler

foreign import ccall unsafe
  "geos_c.h GEOSContext_setErrorHandler_r"
  geos_setErrorHandler :: GEOSContextHandle_t -> GEOSMessageHandler -> IO GEOSMessageHandler

-- Info

foreign import ccall unsafe
  "geos_c.h GEOSGetSRID_r"
  geos_GetSRID :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

foreign import ccall unsafe

  "geos_c.h GEOSSetSRID_r"
  geos_SetSRID :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CInt -> IO ()

foreign import ccall unsafe
  "geos_c.h GEOSGeom_getCoordSeq_r"
  geos_GetCoordSeq :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSCoordSequence)

foreign import ccall unsafe
  "geos_c.h GEOSGeomTypeId_r"
  geos_GeomTypeId :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSGeomType_r"
  geos_GeomType :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr CChar)

foreign import ccall unsafe
  "geos_c.h GEOSNormalize_r"
  geos_Normalize :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

  -- polygons
foreign import ccall unsafe
  "geos_c.h GEOSGetInteriorRingN_r"
  geos_GetInteriorRingN :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CInt -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGetExteriorRing_r"
  geos_GetExteriorRing :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGetNumInteriorRings_r"
  geos_GetNumInteriorRings :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt 
-- multigeometries
foreign import ccall unsafe
  "geos_c.h GEOSGetNumGeometries_r"
  geos_GetNumGeometries :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt 

foreign import ccall unsafe
  "geos_c.h GEOSGetGeometryN_r"
  geos_GetGeometryN :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CInt -> IO (Ptr GEOSGeometry) 

  
-- Linear Referencing
foreign import ccall unsafe
  "geos_c.h GEOSProject_r"
  geos_Project :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CDouble
--- 
foreign import ccall unsafe
  "geos_c.h GEOSInterpolate_r"
  geos_Interpolate :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSProjectNormalized_r"
  geos_ProjectNormalized :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CDouble

foreign import ccall unsafe
  "geos_c.h GEOSInterpolateNormalized_r"
  geos_InterpolateNormalized :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> IO (Ptr GEOSGeometry)

 -- Coord Sequence  -- return 0 on exception
 --
foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_create_r"
  geos_CoordSeqCreate :: GEOSContextHandle_t -> CUInt -> CUInt -> IO (Ptr GEOSCoordSequence)  


foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_clone_r"
  geos_CoordSeqClone :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSCoordSequence )

foreign import ccall unsafe
  "geos_c.h &GEOSCoordSeq_destroy_r" 
  geos_CoordSeqDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO ())

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_setX_r"
  geos_CoordSeqSetX :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_setY_r"
  geos_CoordSeqSetY :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_setZ_r"
  geos_CoordSeqSetZ :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CDouble -> IO CInt 

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_setOrdinate_r"
  geos_CoordSeqSetOrdinate :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CUInt -> CDouble -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_getX_r"
  geos_CoordSeqGetX :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_getY_r"
  geos_CoordSeqGetY :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_getZ_r"
  geos_CoordSeqGetZ :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_getOrdinate_r"
  geos_CoordSeqGetOrdinate :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> CUInt -> CUInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_getSize_r"
  geos_CoordSeqGetSize :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> Ptr CUInt -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSGetNumCoordinates_r"
  geos_GetNumCoordinates :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSCoordSeq_getDimensions_r"
  geos_CoordSeqGetDimensions :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> Ptr CUInt -> IO CInt

foreign import ccall unsafe
  "geos_c.h &GEOSGeom_destroy_r"
  geos_GeomDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSGeometry -> IO ())

foreign import ccall unsafe
  "geos_c.h GEOSGeom_clone_r"
  geos_GeomClone :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

-- Geometry Constructors.
-- * GEOSCoordSequence* arguments will become ownership of the returned object.
-- * All functions return NULL on exception.

foreign import ccall unsafe
  "geos_c.h GEOSGeom_createPoint_r"
  geos_GeomCreatePoint :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGeom_createEmptyPoint_r"
  geos_GeomCreateEmptyPoint :: GEOSContextHandle_t -> IO (Ptr GEOSGeometry)
  
foreign import ccall unsafe
  "geos_c.h GEOSGeom_createLinearRing_r"
  geos_GeomCreateLinearRing ::  GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGeom_createLineString_r"
  geos_GeomCreateLineString ::  GEOSContextHandle_t -> Ptr GEOSCoordSequence -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGeom_createEmptyLineString_r"
  geos_GeomCreateEmptyLineString :: GEOSContextHandle_t -> IO (Ptr GEOSGeometry)

--Second argument is an array of GEOSGeometry* objects.
--The caller remains owner of the array, but pointed-to
--objects become ownership of the returned GEOSGeometry.

foreign import ccall unsafe
  "geos_c.h GEOSGeom_createEmptyPolygon_r"
  geos_GeomCreateEmptyPolygon :: GEOSContextHandle_t -> IO (Ptr GEOSGeometry)

  -- todo: this might not work as a plain pointer
foreign import ccall unsafe
  "geos_c.h GEOSGeom_createPolygon_r"
  geos_GeomCreatePolygon :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr (Ptr GEOSGeometry) -> CUInt -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGeom_createCollection_r"
  geos_GeomCreateCollection :: GEOSContextHandle_t -> CInt -> Ptr (Ptr GEOSGeometry) -> CUInt -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGeom_createEmptyCollection_r"
  geos_GeomCreateEmptyCollection :: GEOSContextHandle_t -> CInt -> IO (Ptr GEOSGeometry)

----------
--- Buffer
---------

newtype BufferCapStyle = BufferCapStyle { unBufferCapStyle :: CInt }
    deriving (Eq,Show)

#{ enum BufferCapStyle, BufferCapStyle,
    capRound = 1 
  , capFlat = 2
  , capSquare = 3
}

newtype BufferJoinStyle = BufferJoinStyle { unBufferJoinStyle :: CInt }
    deriving (Eq, Show)

#{ enum BufferJoinStyle, BufferJoinStyle,
    joinRound = 1
  , joinMitre = 2
  , joinBevel = 3
}
foreign import ccall unsafe
  "geos_c.h GEOSBuffer_r"
  geos_Buffer :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> CInt -> IO (Ptr GEOSGeometry) 

foreign import ccall unsafe
  "geos_c.h GEOSBufferParams_create_r"
  geos_BufferParamsCreate :: GEOSContextHandle_t -> IO (Ptr GEOSBufferParams) 

foreign import ccall unsafe
  "geos_c.h &GEOSBufferParams_destroy_r"
  geos_BufferParamsDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSBufferParams -> IO ())

foreign import ccall unsafe
  "geos_c.h GEOSBufferParams_setEndCapStyle_r"
  geos_BufferParamsSetEndCapStyle :: GEOSContextHandle_t -> Ptr GEOSBufferParams -> CInt -> IO CInt 

foreign import ccall unsafe
  "geos_c.h GEOSBufferParams_setJoinStyle_r"
  geos_BufferParamsSetJoinStyle :: GEOSContextHandle_t -> Ptr GEOSBufferParams -> CInt -> IO CInt 

foreign import ccall unsafe
  "geos_c.h GEOSBufferParams_setMitreLimit_r"
  geos_BufferParamsSetMitreLimit :: GEOSContextHandle_t -> Ptr GEOSBufferParams -> CDouble -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSBufferParams_setQuadrantSegments_r"
  geos_BufferParamsSetQuadrantSegments :: GEOSContextHandle_t -> Ptr GEOSBufferParams -> CInt -> IO CInt 

foreign import ccall unsafe
  "geos_c.h GEOSBufferParams_setSingleSided_r"
  geos_BufferParamsSetSingleSided :: GEOSContextHandle_t -> Ptr GEOSBufferParams -> CInt -> IO CInt

foreign import ccall unsafe
  "geos_c.h GEOSBufferWithParams_r"
  geos_BufferWithParams :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSBufferParams -> CDouble -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSBufferWithStyle_r"
  geos_BufferWithStyle :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> CInt -> CInt -> CInt -> CDouble -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSOffsetCurve_r"
  geos_OffsetCurve :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> CInt -> CInt -> CDouble -> IO (Ptr GEOSGeometry)

-----------
--- Topology
----------
foreign import ccall unsafe
  "geos_c.h GEOSEnvelope_r"
  geos_Envelope :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry) 
  
foreign import ccall unsafe
  "geos_c.h GEOSIntersection_r"
  geos_Intersection :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSIntersection_r"
  geos_ConvexHull :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSDifference_r"
  geos_Difference :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSSymDifference_r"
  geos_SymDifference :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSBoundary_r"
  geos_Boundary :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSUnion_r"
  geos_Union :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSUnaryUnion_r"
  geos_UnaryUnion :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSGetCentroid_r"
  geos_GetCentroid :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSPointOnSurface_r"
  geos_PointsOnSurface :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSNode_r"
  geos_Node :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)


foreign import ccall unsafe
  "geos_c.h GEOSDelaunayTriangulation_r"
  geos_DelaunayTriangulation :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> CInt -> IO (Ptr GEOSGeometry)

{-foreign import ccall unsafe-}
  {-"geos_c.h GEOSVoronoiDiagram_r"-}
  {-geos_VoronoiDiagram :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> CDouble -> CInt -> IO (Ptr GEOSGeometry)-}

-----
--Binary Predicates.
-----

foreign import ccall unsafe
  "geos_c.h GEOSDisjoint_r"
  geos_Disjoint :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSTouches_r"
  geos_Touches :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSCrosses_r"
  geos_Crosses :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar
  
foreign import ccall unsafe
  "geos_c.h GEOSIntersects_r"
  geos_Intersects :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSWithin_r"
  geos_Within :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSContains_r"
  geos_Contains :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSOverlaps_r"
  geos_Overlaps :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSEquals_r"
  geos_Equals :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSEqualsExact_r"
  geos_EqualsExact :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSCovers_r"
  geos_Covers :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSCoveredBy_r"
  geos_CoveredBy :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

--- prepared Geometries

---  Readers / Writers
foreign import ccall unsafe
  "geos_c.h GEOSWKBReader_create_r"
  geos_WKBReaderCreate :: GEOSContextHandle_t -> IO (Ptr GEOSWKBReader) 

foreign import ccall unsafe
  "geos_c.h &GEOSWKBReader_destroy_r"
  geos_WKBReaderDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSWKBReader -> IO ())

foreign import ccall unsafe
  "geos_c.h GEOSWKBReader_read_r"
  geos_WKBReaderRead :: GEOSContextHandle_t -> Ptr GEOSWKBReader -> CString  -> CSize -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSWKBReader_readHEX_r"
  geos_WKBReaderReadHex :: GEOSContextHandle_t -> Ptr GEOSWKBReader -> CString -> CSize -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "geos_c.h GEOSWKBWriter_create_r"
  geos_WKBWriterCreate :: GEOSContextHandle_t -> IO (Ptr GEOSWKBWriter) 

foreign import ccall unsafe
  "geos_c.h &GEOSWKBWriter_destroy_r"
  geos_WKBWriterDestroy :: FunPtr ( GEOSContextHandle_t -> Ptr GEOSWKBWriter -> IO ())

  -- caller owns results
foreign import ccall unsafe
  "geos_c.h GEOSWKBWriter_write_r"
  geos_WKBWriterWrite :: GEOSContextHandle_t -> Ptr GEOSWKBWriter -> Ptr GEOSGeometry -> Ptr CSize -> IO CString

  -- caller owns results
foreign import ccall unsafe
  "geos_c.h GEOSWKBWriter_writeHEX_r"
  geos_WKBWriterWriteHex :: GEOSContextHandle_t -> Ptr GEOSWKBWriter -> Ptr GEOSGeometry -> Ptr CSize -> IO CString

--TODO: 1085 finish writer methods

-- following return 0 on exception, 1 otherwise
foreign import ccall unsafe 
  "geos_c.h GEOSArea_r"
  geos_Area :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr CDouble -> IO CInt 

foreign import ccall unsafe 
  "geos_c.h GEOSLength_r"
  geos_Length :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr CDouble -> IO CInt 

foreign import ccall unsafe 
  "geos_c.h GEOSDistance_r"
  geos_Distance :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> Ptr CDouble -> IO CInt 

foreign import ccall unsafe 
  "geos_c.h GEOSHausdorffDistance_r"
  geos_HausdorffDistance :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> Ptr CDouble -> IO CInt 

foreign import ccall unsafe 
  "geos_c.h GEOSHausdorffDistanceDensify_r"
  geos_HausdorffDistanceDensify :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> CDouble -> Ptr CDouble -> IO CInt 

foreign import ccall unsafe 
  "geos_c.h GEOSGeomGetLength_r"
  geos_GeomGetLength :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr CDouble -> IO CInt 

foreign import ccall unsafe 
  "geos_c.h GEOSNearestPoints_r"
  geos_NearestPoints :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO (Ptr GEOSCoordSequence)

-- | Prepared Geometries
foreign import ccall unsafe
  "geos_c.h GEOSPrepare_r"
  geos_Prepare :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSPreparedGeometry)

foreign import ccall unsafe
  "geos_c.h &GEOSPreparedGeom_destroy_r"
  geos_PreparedGeomDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> IO ())

foreign import ccall unsafe
  "geos_c.h GEOSPreparedContains_r"
  geos_PreparedContains :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedContainsProperly_r"
  geos_PreparedContainsProperly :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedCoveredBy_r"
  geos_PreparedCoveredBy :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedCovers_r"
  geos_PreparedCovers :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedCrosses_r"
  geos_PreparedCrosses :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedDisjoint_r"
  geos_PreparedDisjoint :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedIntersects_r"
  geos_PreparedIntersects :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedOverlaps_r"
  geos_PreparedOverlaps :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedTouches_r"
  geos_PreparedTouches :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "geos_c.h GEOSPreparedWithin_r"
  geos_PreparedWithin :: GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar
