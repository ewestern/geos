{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Geos.Topology
  ( envelope
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
  , minimumClearance
  , minimumClearanceLine
  , minimumWidth
  )
where

import           Control.Monad
import qualified Data.Vector                   as V
import           Data.Geometry.Geos.Raw.Base
import           Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Topology
                                               as R
import qualified Data.Geometry.Geos.Raw.Geometry
                                               as RG


geo_1
  :: (RG.GeomConst -> Geos RG.GeomConst) -> Geometry a -> Maybe (Some Geometry)
geo_1 f g = runGeosM $ do
  geo <- convertGeometryToRaw g
  convertGeometryFromRaw =<< f geo

geo_2
  :: (RG.GeomConst -> RG.GeomConst -> Geos RG.GeomConst)
  -> Geometry a
  -> Geometry b
  -> Maybe (Some Geometry)
geo_2 f g1 g2 = runGeosM $ do
  g1' <- convertGeometryToRaw g1
  g2' <- convertGeometryToRaw g2
  convertGeometryFromRaw =<< f g1' g2'

-- | Returns a Polygon that represents the bounding envelope of this geometry. Note that it can also return a Point if the input geometry is a point.
envelope :: Geometry a -> Maybe (Some Geometry)
envelope = geo_1 R.envelope

-- | Returns a Geometry representing the points shared by both geometries.
intersection :: Geometry a -> Geometry b -> Maybe (Some Geometry)
intersection = geo_2 R.intersection

-- | Returns the smallest Polygon that contains all the points in the geometry.
convexHull :: Geometry a -> Maybe (Geometry Polygon)
convexHull g = ensurePolygon =<< geo_1 R.convexHull g

-- | Returns a Geometry representing the points making up this geometry that do not make up other.
difference :: Geometry a -> Geometry b -> Maybe (Some Geometry)
difference = geo_2 R.difference

-- | Returns a Geometry combining the points in this geometry not in other, and the points in other not in this geometry.
symmetricDifference :: Geometry a -> Geometry b -> Maybe (Some Geometry)
symmetricDifference = geo_2 R.symmetricDifference

boundary :: Geometry a -> Maybe (Some Geometry)
boundary = geo_1 R.boundary

-- | Returns a Geometry representing all the points in both geometries.

{-| Computes the union of all the elements of this geometry. Heterogeneous GeometryCollections are fully supported.

The result obeys the following contract:

Unioning a set of LineStrings has the effect of fully noding and dissolving the linework.
Unioning a set of Polygons will always return a Polygonal geometry (unlike {link #union(Geometry)}, which may return geometrys of lower dimension if a topology collapse occurred.
-}
union :: Geometry a -> Geometry b -> Maybe (Some Geometry)
union = geo_2 R.union


unaryUnion :: Geometry a -> Maybe (Some Geometry)
unaryUnion = geo_1 R.unaryUnion

-- | Computes and returns a Point guaranteed to be on the interior of this geometry.
pointOnSurface :: Geometry a -> Maybe (Geometry Point)
pointOnSurface = ensurePoint <=< geo_1 R.pointOnSurface

-- | Returns a Point object representing the geometric center of the geometry. The point is not guaranteed to be on the interior of the geometry.
centroid :: Geometry a -> Maybe (Geometry Point)
centroid = ensurePoint <=< geo_1 R.centroid

node :: Geometry a -> Maybe (Some Geometry)
node = geo_1 R.node

-- | Return a Delaunay triangulation of the vertex of the given geometry @g@, where @tol@ is  the snapping tolerance to use.
delaunayTriangulation
  :: Geometry a -> Double -> Maybe (Geometry MultiLineString)
delaunayTriangulation g d =
  ensureMultiLineString =<< geo_1 (`R.delaunayTriangulation` d) g

-- | Returns the Voronoi polygons of a set of Vertices given as input
voronoiDiagram
  :: Geometry a -> Maybe (Geometry b) -> Double -> Bool -> Some Geometry
voronoiDiagram g menv tol onlyEdges = runGeos $ do
  g' :: RG.GeomConst <- convertGeometryToRaw g
  env'               <- traverse convertGeometryToRaw menv
  convertGeometryFromRaw =<< R.voronoiDiagram g' env' tol onlyEdges

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
polygonize :: V.Vector (Geometry a) -> Maybe (Geometry Polygon)
polygonize vec =
  ensurePolygon
    =<< (runGeosM $ do
          listGeom :: [RG.Geom] <- traverse convertGeometryToRaw $ V.toList vec
          pgon                  <- R.polygonize listGeom
          convertGeometryFromRaw pgon
        )

{-
 Returns the minimum rotated rectangular POLYGON which encloses the input geometry. The rectangle has width equal to the minimum diameter, and a longer length. If the convex hull of the input is degenerate (a line or point) a LINESTRING or POINT is returned. The minimum rotated rectangle can be used as an extremely generalized representation for the given geometry.
-}

minimumRotatedRectangle :: Geometry a -> Maybe (Geometry Polygon)
minimumRotatedRectangle = ensurePolygon <=< geo_1 R.minimumRotatedRectangle

-- | Returns a LINESTRING geometry which represents the minimum diameter of the geometry. The minimum diameter is defined to be the width of the smallest band that contains the geometry, where a band is a strip of the plane defined  by two parallel lines. This can be thought of as the smallest hole that the geometry  can be moved through, with a single rotation.
minimumWidth :: Geometry a -> Maybe (Geometry LineString)
minimumWidth = ensureLineString <=< geo_1 R.minimumWidth


minimumClearance :: Geometry a -> Maybe Double
minimumClearance geom = runGeosM $ do
  raw :: RG.Geom <- convertGeometryToRaw geom
  R.minimumClearance raw


minimumClearanceLine :: Geometry a -> Maybe (Geometry LineString)
minimumClearanceLine = ensureLineString <=< geo_1 R.minimumClearanceLine
