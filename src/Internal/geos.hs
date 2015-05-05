{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Geos.Internal where

import Foreign
import Foreign.C

#include <geos_c.h>
{-http://geos.osgeo.org/doxygen/geos__c_8h_source.html-}

#{ enum GEOSGeomTypes

}

 enum GEOSGeomTypes {
   00125     GEOS_POINT,
   00126     GEOS_LINESTRING,
   00127     GEOS_LINEARRING,
   00128     GEOS_POLYGON,
   00129     GEOS_MULTIPOINT,
   00130     GEOS_MULTILINESTRING,
   00131     GEOS_MULTIPOLYGON,
   00132     GEOS_GEOMETRYCOLLECTION
   00133 };
