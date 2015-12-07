{-# LANGUAGE CPP #-}
module GEOS.Raw.Topology (
    envelope
  , intersection
  , convexHull
  , difference
  , symmetricDifference
  , boundary
  , union
  , unaryUnion
  , pointOnSurface
  , getCentroid
  , node
  , delaunayTriangulation
) where
import qualified GEOS.Raw.Internal as I
import GEOS.Raw.Base
import qualified GEOS.Raw.Geometry as R
import Foreign

geo_1 :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry)) 
          -> String 
          -> R.Geometry 
          -> Geos R.Geometry 
geo_1 f s g =  withGeos $ \h ->  do
  g' <- throwIfNull s $ 
        R.withGeometry g $ \gp -> 
          f h gp 
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g'
  return $ R.Geometry fp

geo_2 :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry))
          -> String
          -> R.Geometry
          -> R.Geometry
          -> Geos R.Geometry
geo_2 f s g1 g2  = withGeos $ \h -> do
  g <- throwIfNull s $ 
        R.withGeometry g1 $ \gp1 ->
         R.withGeometry g2 $ \gp2 ->
            f h gp1 gp2 
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g
  return $ R.Geometry fp


envelope :: R.Geometry -> Geos R.Geometry 
envelope = geo_1 I.geos_Envelope "envelope" 

intersection :: R.Geometry -> R.Geometry -> Geos R.Geometry
intersection = geo_2 I.geos_Intersection "intersection"

convexHull :: R.Geometry -> Geos R.Geometry
convexHull = geo_1 I.geos_ConvexHull "convexHull"

difference :: R.Geometry -> R.Geometry -> Geos R.Geometry
difference = geo_2 I.geos_Difference "difference"

symmetricDifference :: R.Geometry -> R.Geometry -> Geos R.Geometry
symmetricDifference = geo_2 I.geos_SymDifference "symmetricDifference"

boundary :: R.Geometry -> Geos R.Geometry
boundary = geo_1 I.geos_Boundary "boundary"

union :: R.Geometry -> R.Geometry -> Geos R.Geometry
union = geo_2 I.geos_Union "union"

unaryUnion :: R.Geometry -> Geos R.Geometry
unaryUnion = geo_1 I.geos_UnaryUnion "unaryUnion"

pointOnSurface :: R.Geometry -> Geos R.Geometry
pointOnSurface = geo_1 I.geos_PointsOnSurface "pointOnSurface"

getCentroid :: R.Geometry -> Geos R.Geometry
getCentroid = geo_1 I.geos_GetCentroid "getCentroid"

node :: R.Geometry -> Geos R.Geometry
node = geo_1 I.geos_Node "node"

-- | Return a Delaunay triangulation of the vertex of the given geometry @g@, where @tol@ is  the snapping tolerance to use and @oe@ indicates that the function should return a MultiLineString, rather than a GeometryCollection containing triangular Polygons.

delaunayTriangulation :: R.Geometry -> Double -> Bool -> Geos R.Geometry 
delaunayTriangulation g tol oe = withGeos $ \h -> do
  g' <- throwIfNull "delaunayTriangulation" $ 
        R.withGeometry g $ \gp ->
          I.geos_DelaunayTriangulation h gp (realToFrac tol) $ fromBool oe 
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g'
  return $ R.Geometry fp
            
#if GEOS_VERSION_MAJOR > 3 && GEOS_VERSION_MINOR > 4
-- | 
-- TODO: make env Maybe Geometry
voronoiDiagram :: R.Geometry -> R.Geometry -> Double -> Bool -> Geos R.Geometry
voronoiDiagram g env tol oe = withGeos $ \h ->  do
  g <- throwIfNull "voronoiDiagram" $ 
        R.withGeometry g $ \gp -> 
          R.withGeometry env $ \ep -> 
            I.geos_VoronoiDiagram hp gp ep (realToFrac tol) $ fromBool oe 
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g
  return $ R.Geometry g
#endif
