{-# LINE 1 "Internal/geos.hs" #-}
{-# LINE 1 "geos.hsc" #-}
{-# LINE 2 "Internal/geos.hs" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
{-# LINE 2 "geos.hsc" #-}
module Geos.Internal where

import Foreign
import Foreign.C
import Foreign.C.String


{-# LINE 9 "geos.hsc" #-}
{-http://geos.osgeo.org/doxygen/geos__c_8h_source.html-}
newtype GEOSGeomType = GEOSGeomType { unGEOSGeomType :: CInt }
    deriving (Eq,Show)
newtype GEOSByteOrder = GEOSByteOrder { unGEOSByteOrder :: CInt}
  deriving (Eq, Show)

data GEOSContextHandle
data GEOSGeometry
data GEOSCoordinateSequence
data GEOSSTRtree
data GEOSBufferParams

type GEOSMessageHandler = CString -> ()

foreign import ccall unsafe 
  "GEOS/geos_c.h initGEOS_r"
   geos_initGEOS_r :: FunPtr GeoMessageHandler -> FunPtr GeoMessageHandler -> IO (Ptr GeoContextHandle)

foreign import ccall unsafe
  "GEOS/geos_c.h finishGEOS_r"
  geos_finishGEOS_r :: GEOSContextHandle -> IO ()

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_create_r"
  geos_coordSeqCreate :: GEOSContextHandle -> CUInt -> CUInt -> IO (Ptr GEOSCoordSequence)  

foreign import ccall unsafe
  "GEOS/geos_c.h GEOSCoordSeq_destroy_r" 
  geos_coordSeqDestroy :: GEOSContextHandle -> (Ptr GEOSCoordSequence) -> IO ()

geos_point  :: GEOSGeomType
geos_point  = GEOSGeomType 0
geos_lineString  :: GEOSGeomType
geos_lineString  = GEOSGeomType 1
geos_polygon  :: GEOSGeomType
geos_polygon  = GEOSGeomType 3
geos_multiPoint  :: GEOSGeomType
geos_multiPoint  = GEOSGeomType 4
geos_multiLineString  :: GEOSGeomType
geos_multiLineString  = GEOSGeomType 4
geos_multiPolygon  :: GEOSGeomType
geos_multiPolygon  = GEOSGeomType 6
geos_geometryCollection  :: GEOSGeomType
geos_geometryCollection  = GEOSGeomType 7

{-# LINE 48 "geos.hsc" #-}

geos_bigEndian  :: GEOSByteOrder
geos_bigEndian  = GEOSByteOrder 0
geos_littleEndian  :: GEOSByteOrder
geos_littleEndian  = GEOSByteOrder 1

{-# LINE 53 "geos.hsc" #-}


