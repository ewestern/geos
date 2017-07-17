{-# LANGUAGE CPP #-}
module Data.Geometry.Geos.Raw.Topology (
    envelope
  , intersection
  , convexHull
  , difference
  , symmetricDifference
  , boundary
  , union
  , unaryUnion
  , pointOnSurface
  , centroid
  , node
  , delaunayTriangulation
) where
import qualified Data.Geometry.Geos.Raw.Internal as I
import Data.Geometry.Geos.Raw.Base
import qualified Data.Geometry.Geos.Raw.Geometry as R
import Foreign

geo_1 :: R.Geometry a 
      => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry)) 
      -> String 
      -> a
      -> Geos a
geo_1 f s g = 
  withGeos $ \h -> 
    R.withGeometry g $ \gp -> do
      ptr <- throwIfNull s $ f h gp
      R.constructGeometry h ptr

geo_2 :: R.Geometry a
      => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry))
      -> String
      -> a
      -> a
      -> Geos a
geo_2 f s g1 g2  = do
  withGeos $ \h -> 
    R.withGeometry g1 $ \gp -> 
      R.withGeometry g2 $ \gp2 -> do
        ptr <- throwIfNull s $ f h gp gp2
        R.constructGeometry h ptr


envelope :: R.Geometry a => a -> Geos a
envelope = geo_1 I.geos_Envelope "envelope" 

intersection :: R.Geometry a => a -> a -> Geos a
intersection = geo_2 I.geos_Intersection "intersection"

convexHull :: R.Geometry a => a -> Geos a
convexHull = geo_1 I.geos_ConvexHull "convexHull"

difference :: R.Geometry a => a -> a -> Geos a
difference = geo_2 I.geos_Difference "difference"

symmetricDifference :: R.Geometry a => a -> a-> Geos a
symmetricDifference = geo_2 I.geos_SymDifference "symmetricDifference"

boundary :: R.Geometry a => a -> Geos a
boundary = geo_1 I.geos_Boundary "boundary"

union :: R.Geometry a => a -> a -> Geos a
union = geo_2 I.geos_Union "union"

unaryUnion :: R.Geometry a => a -> Geos a
unaryUnion = geo_1 I.geos_UnaryUnion "unaryUnion"

pointOnSurface :: R.Geometry a => a -> Geos a
pointOnSurface = geo_1 I.geos_PointsOnSurface "pointOnSurface"

centroid :: R.Geometry a => a -> Geos a
centroid = geo_1 I.geos_GetCentroid "getCentroid"

node :: R.Geometry a => a -> Geos a
node = geo_1 I.geos_Node "node"

-- | Return a Delaunay triangulation of the vertex of the given geometry @g@, where @tol@ is  the snapping tolerance to use.
delaunayTriangulation :: R.Geometry a => a -> Double -> Geos a
delaunayTriangulation g tol = do
  withGeos $ \h -> do
    R.withGeometry g $ \gp -> do 
      ptr <- throwIfNull "delaunayTriangulation" $ I.geos_DelaunayTriangulation h gp (realToFrac tol) $ fromBool True
      R.constructGeometry h ptr
            
#if GEOS_VERSION_MAJOR > 3 && GEOS_VERSION_MINOR > 4
-- | 
-- TODO: make env Maybe Geometry
voronoiDiagram :: R.Geometry a => a -> Maybe a -> Double -> Bool -> Geos a
voronoiDiagram g menv tol oe = do
  withGeos $ \h ->
    R.withGeometry g $ \gp -> 
      R.withMaybeGeometry env $ \ep -> 
        ptr <- throwIfNull "voronoiDiagram" $ I.geos_VoronoiDiagram hp gp ep (realToFrac tol) $ fromBool oe 
        R.constructGeometry h ptr
#endif
