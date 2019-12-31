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
  , voronoiDiagram
  , polygonize
  , minimumRotatedRectangle
  , minimumWidth
  , minimumClearanceLine
  , minimumClearance
) where
import qualified Data.Geometry.Geos.Raw.Internal as I
import Data.Geometry.Geos.Raw.Base
import qualified Data.Geometry.Geos.Raw.Geometry as R

import Foreign hiding (throwIfNull, throwIf)

geo_1 :: R.Geometry a 
      => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry)) 
      -> String 
      -> a
      -> Geos a
geo_1 f s g = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' s $ R.withGeometry g $ f h 
  traverse (R.constructGeometry h) eitherPtr

geo_2 :: R.Geometry a
      => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO (Ptr I.GEOSGeometry))
      -> String
      -> a
      -> a
      -> Geos a
geo_2 f s g1 g2  =  withGeos' $ \h -> do
  eitherPtr <- throwIfNull' s $  R.withGeometry g1 $ \gp -> 
      R.withGeometry g2 (f h gp)
  traverse (R.constructGeometry h) eitherPtr



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

{-
 Polygonizes a set of Geometries which contain linework that
 represents the edges of a planar graph.
 
 All types of Geometry are accepted as input; the constituent
 linework is extracted as the edges to be polygonized.
 
 The edges must be correctly noded; that is, they must only meet
 at their endpoints.  The set of extracted polygons
 is guaranteed to be edge-disjoint. This is useful when it is known
 that the input lines form a valid polygonal geometry (which may
 include holes or nested polygons).
-}
polygonize :: R.Geometry a => [ a ] -> Geos a
polygonize geoms = withGeos' $ \h -> alloca $ \arrPtr -> do
  _ <- traverse (writeIndexed arrPtr) $ [0 ..] `zip` geoms
  eitherPtr <- throwIfNull' "polygonize" $ I.geos_Polygonize_valid h arrPtr $ fromIntegral (length geoms)
  traverse (R.constructGeometry h) eitherPtr 
  where
    writeIndexed arrayPtr (idx, geom) = R.withGeometry geom $ \geoPtr -> pokeElemOff arrayPtr idx geoPtr


{-
 Returns the minimum rotated rectangular POLYGON which encloses the input geometry. The rectangle has width equal to the minimum diameter, and a longer length. If the convex hull of the input is degenerate (a line or point) a LINESTRING or POINT is returned. The minimum rotated rectangle can be used as an extremely generalized representation for the given geometry.
-}
minimumRotatedRectangle :: R.Geometry a => a -> Geos a
minimumRotatedRectangle = geo_1 I.geos_MinimumRotatedRectangle "minimumRotatedRectangle"

-- | Returns a LINESTRING geometry which represents the minimum diameter of the geometry. The minimum diameter is defined to be the width of the smallest band that contains the geometry, where a band is a strip of the plane defined  by two parallel lines. This can be thought of as the smallest hole that the geometry  can be moved through, with a single rotation.
minimumWidth :: R.Geometry a => a -> Geos a
minimumWidth = geo_1 I.geos_MinimumWidth "minimumWidth"

-- | Returns a LineString whose endpoints define the minimum clearance of a geometry. If the geometry has no minimum clearance, an empty LineString will be returned.

minimumClearanceLine :: R.Geometry a => a -> Geos a
minimumClearanceLine = geo_1 I.geos_MinimumClearanceLine "minimumClearanceLine"

-- | Computes the minimum clearance of a geometry.  The minimum clearance is the smallest amount by which  a vertex could be move to produce an invalid polygon, a non-simple linestring, or a multipoint with  repeated points.  If a geometry has a minimum clearance of 'eps', it can be said that:
-- |  -  No two distinct vertices in the geometry are separated by less than 'eps'
-- |  -  No vertex is closer than 'eps' to a line segment of which it is not an endpoint.
--  If the minimum clearance cannot be defined for a geometry (such as with a single point, or a multipoint whose points are identical, a value of Infinity will be calculated.
minimumClearance :: R.Geometry a => a -> Geos Double
minimumClearance geom = withGeos' $ \h ->
  R.withGeometry geom $ \gptr ->  alloca $ \dptr ->  do
    eitherInt <-  throwIf' (0 /=) (mkErrorMessage "minimumClearance") $ I.geos_MinimumClearance h gptr dptr
    traverse (\_ -> realToFrac <$> peek dptr) eitherInt


-- | Return a Delaunay triangulation of the vertex of the given geometry @g@, where @tol@ is  the snapping tolerance to use.
delaunayTriangulation :: R.Geometry a 
                      => a 
                      -> Double 
                      -> Bool
                      -> Geos a
delaunayTriangulation g tol onlyEdges = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "delaunayTriangulation" $ R.withGeometry g $ \gp -> 
      I.geos_DelaunayTriangulation h gp (realToFrac tol) $ fromBool onlyEdges
  traverse (R.constructGeometry h) eitherPtr
            
voronoiDiagram :: R.Geometry a => a -> Maybe a -> Double -> Bool -> Geos a
voronoiDiagram g menv tol oe = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "voronoiDiagram" $ R.withGeometry g $ \gp -> 
    R.withMaybeGeometry menv $ \ep -> I.geos_VoronoiDiagram h gp ep (realToFrac tol) $ fromBool oe 
  traverse (R.constructGeometry h) eitherPtr
