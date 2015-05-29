{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module GEOS.Raw.Internal where

import Foreign
import Foreign.C
import Foreign.C.String

#include <geos_c.h>

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
  geos_GeomType :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr CChar)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSNormalize_r"
  geos_Normalize :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

  -- polygons
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetInteriorRingN_r"
  geos_GetInteriorRingN :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CInt -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetExteriorRing_r"
  geos_GetExteriorRing :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetNumInteriorRings_r"
  geos_GetNumInteriorRings :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt 
-- multigeometries
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetNumGeometries_r"
  geos_GetNumGeometries :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt 

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetGeometryN_r"
  geos_GetGeometryN :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CInt -> IO (Ptr GEOSGeometry) 

  
-- Linear Referencing
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSProject_r"
  geos_Project :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CDouble
--- 
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSInterpolate_r"
  geos_Interpolate :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSProjectNormalized_r"
  geos_ProjectNormalized :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CDouble

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSInterpolateNormalized_r"
  geos_InterpolateNormalized :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> IO (Ptr GEOSGeometry)

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
  geos_GetNumCoordinates :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_getDimensions_r"
  geos_CoordSeqGetDimensions :: GEOSContextHandle_t -> Ptr GEOSCoordSequence -> Ptr CUInt -> IO CInt

foreign import ccall unsafe
  "GEOS/geos_c.h &GEOSGeom_destroy_r"
  geos_GeomDestroy :: FunPtr (GEOSContextHandle_t -> Ptr GEOSGeometry -> IO ())

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_clone_r"
  geos_GeomClone :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

-- Geometry Constructors.
-- * GEOSCoordSequence* arguments will become ownership of the returned object.
-- * All functions return NULL on exception.

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

--Second argument is an array of GEOSGeometry* objects.
--The caller remains owner of the array, but pointed-to
--objects become ownership of the returned GEOSGeometry.


foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createEmptyPolygon_r"
  geos_GeomCreateEmptyPolygon :: GEOSContextHandle_t -> IO (Ptr GEOSGeometry)

  -- todo: this might not work as a plain pointer
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGeom_createPolygon_r"
  geos_GeomCreatePolygon :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr (Ptr GEOSGeometry) -> CUInt -> IO (Ptr GEOSGeometry)
--

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
  "GEOS/geos_c.h GEOSUnaryUnion_r"
  geos_UnaryUnion :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSGetCentroid_r"
  geos_GetCentroid :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSPointOnSurface_r"
  geos_PointsOnSurface :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSNode_r"
  geos_Node :: GEOSContextHandle_t -> Ptr GEOSGeometry -> IO (Ptr GEOSGeometry)


-- Return a Delaunay triangulation of the vertex of the given geometry

-- @param g the input geometry whose vertex will be used as "sites"
-- @param tolerance optional snapping tolerance to use for improved robustness
-- @param onlyEdges if non-zero will return a MULTILINESTRING, otherwise it will
--                  return a GEOMETRYCOLLECTION containing triangular POLYGONs.

--return  a newly allocated geometry, or NULL on exception

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSDelaunayTriangulation_r"
  geos_DelaunayTriangulation :: GEOSContextHandle_t -> Ptr GEOSGeometry -> CDouble -> CInt -> IO (Ptr GEOSGeometry)

-- Returns the Voronoi polygons of a set of Vertices given as input
-- @param g the input geometry whose vertex will be used as sites.
-- @param tolerance snapping tolerance to use for improved robustness
-- @param onlyEdges whether to return only edges of the voronoi cells
-- @param env clipping envelope for the returned diagram, automatically
--            determined if NULL.
--            The diagram will be clipped to the larger
--            of this envelope or an envelope surrounding the sites.
 
-- @return a newly allocated geometry, or NULL on exception.

{-foreign import ccall unsafe-}
  {-"GEOS/geos_c.h GEOSVoronoiDiagram_r"-}
  {-geos_VoronoiDiagram :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> CDouble -> CInt -> IO (Ptr GEOSGeometry)-}

-----
--Binary Predicates.
-----

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSDisjoint_r"
  geos_Disjoint :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSTouches_r"
  geos_Touches :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCrosses_r"
  geos_Crosses :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar
  
foreign import ccall unsafe
  "GEOS/geos_c.h GEOSIntersects_r"
  geos_Intersects :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSWithin_r"
  geos_Within :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSContains_r"
  geos_Contains :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSOverlaps_r"
  geos_Overlaps :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSEquals_r"
  geos_Equals :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSEqualsExact_r"
  geos_EqualsExact :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCovers_r"
  geos_Covers :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoveredBy_r"
  geos_CoveredBy :: GEOSContextHandle_t -> Ptr GEOSGeometry -> Ptr GEOSGeometry -> IO CChar

--- prepared Geometries
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
