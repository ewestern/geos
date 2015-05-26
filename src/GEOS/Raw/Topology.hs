module GEOS.Topology where
import qualified GEOS.Internal as I
import GEOS.Wrapper
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr


geo_1 :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry)) -> String -> GEOSHandle -> Geometry -> Geometry 
geo_1 f s h g =  unsafePerformIO $ do
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
          -> Geometry
geo_2 f s h g1 g2  = unsafePerformIO $ do
  g <- throwIfNull s $ withHandle h (\ch -> 
        withGeometry g1 (\gp1 ->
          withGeometry g2 (\gp2 ->
            f ch gp1 gp2 )))
  fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
  return $ Geometry fp


envelope :: GEOSHandle -> Geometry -> Geometry 
envelope = geo_1 I.geos_Envelope "envelope" 

intersection :: GEOSHandle -> Geometry -> Geometry -> Geometry
intersection = geo_2 I.geos_Intersection "intersection"

convexHull :: GEOSHandle -> Geometry -> Geometry
convexHull = geo_1 I.geos_ConvexHull "convexHull"

difference :: GEOSHandle -> Geometry -> Geometry -> Geometry
difference = geo_2 I.geos_Difference "difference"

symmetricDifference :: GEOSHandle -> Geometry -> Geometry -> Geometry
symmetricDifference = geo_2 I.geos_SymDiffference "symmetricDifference"

boundary :: GEOSHandle -> Geometry -> Geometry
boundary = geos_1 I.geos_Boundary "boundary"

union :: GEOSHandle -> Geometry -> Geometry -> Geometry
union = geos_2 I.geos_Union "union"

unaryUnion :: GEOSHandle -> Geometry -> Geometry
unaryUnion = geos_1 I.geos_UnaryUnion "unaryUnion"

pointOnSurface :: GEOSHandle -> Geometry -> Geometry
pointOnSurface = geos_1 I.geos_PointOnSurface "pointOnSurface"

getCentroid :: GEOSHandle -> Geometry -> Geometry
getCentroid = geos_1 I.geos_GetCentroid "getCentroid"

node :: GEOSHandle -> Geometry -> Geometry
node = geos_1 I.geos_Node "node"


delaunayTriangulation :: GEOSHandle  -> Geometry -> Double -> Bool -> Geometry 
delaunayTriangulation h g tol oe = unsafePerformIO $ do
  g <- throwIfNull "delaunayTriangulation" $ withHandle h $ \hp ->
        withGeometry g $ \gp ->
          I.geos_DelaunayTriangulation hp gp (realToFrac) $ case oe of
            True -> 1
            False -> 0
  fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
  return $ Geometry g
            
{-voronoiDiagram :: GEOSHandle -> Geometry -> Geometry -> Double -> Bool -> Geometry-}
{-voronoiDiagram h g env tol oe = unsafePerformIO $ do-}
  {-g <- throwIfNull "voronoiDiagram" $ withHandle h $ \hp -> -}
        {-withGeometry g $ \gp -> -}
          {-withGeometry env $ \ep -> -}
            {-I.geos_VoronoiDiagram hp gp ep (realToFrac tol) $ case oe of-}
              {-True -> 1-}
              {-False -> 0-}
  {-fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g-}
  {-return $ Geometry g-}
