module GEOS.Topology where
import qualified GEOS.Internal as I
import GEOS.Wrapper
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr


geo_1 :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry)) -> String -> GEOSHandle -> Geometry -> IO Geometry 
geo_1 f s h g =  do
  g <- throwIfNull s $ withHandle h (\ch -> 
        withGeometry g (\gp -> 
          f ch gp ))
  fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
  return $ Geometry fp

geo_2 :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry))
          -> String
          -> GEOSHandle
          -> Geometry
          -> Geometry
          -> IO Geometry
geo_2 f s h g1 g2  = do
  g <- throwIfNull s $ withHandle h (\ch -> 
        withGeometry g1 (\gp1 ->
          withGeometry g2 (\gp2 ->
            f ch gp1 gp2 )))
  fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
  return $ Geometry fp


envelope :: GEOSHandle -> Geometry -> IO Geometry 
envelope = geo_1 I.geos_Envelope "envelope" 


intersection :: GEOSHandle -> Geometry -> Geometry -> IO Geometry
intersection = geo_2 I.geos_Intersection "intersection"

convexHull :: GEOSHandle -> Geometry -> IO Geometry
convexHull = geo_1 I.geos_ConvexHull "convexHull"
