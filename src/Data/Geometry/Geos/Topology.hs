{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Geos.Topology (
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
) where
import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Topology as R
import qualified Data.Geometry.Geos.Raw.Geometry as RG


geo_1 ::( RG.GeomConst -> Geos RG.GeomConst )
      -> Geometry a
      -> Some Geometry
geo_1 f g = runGeos $ do
    geo <- convertGeometryToRaw g
    convertGeometryFromRaw =<< f geo


geo_2 :: (RG.GeomConst -> RG.GeomConst -> Geos RG.GeomConst )
      -> Geometry a
      -> Geometry b
      -> Some Geometry
geo_2 f g1 g2 = runGeos $ do
  g1' <- convertGeometryToRaw g1
  g2' <- convertGeometryToRaw g2
  convertGeometryFromRaw =<< f g1' g2'

-- | Returns a Polygon that represents the bounding envelope of this geometry. Note that it can also return a Point if the input geometry is a point.
envelope :: Geometry a -> Some Geometry
envelope = geo_1 R.envelope

-- | Returns a Geometry representing the points shared by both geometries.
intersection :: Geometry a -> Geometry b -> Some Geometry
intersection = geo_2 R.intersection

-- | Returns the smallest Polygon that contains all the points in the geometry.
convexHull :: Geometry a -> Geometry Polygon
convexHull g = ensurePolygon $ geo_1 R.convexHull g

-- | Returns a Geometry representing the points making up this geometry that do not make up other.
difference :: Geometry a -> Geometry b -> Some Geometry
difference = geo_2 R.difference

-- | Returns a Geometry combining the points in this geometry not in other, and the points in other not in this geometry.
symmetricDifference :: Geometry a -> Geometry b -> Some Geometry
symmetricDifference = geo_2 R.symmetricDifference

boundary :: Geometry a -> Some Geometry
boundary = geo_1 R.boundary

-- | Returns a Geometry representing all the points in both geometries.

{-| Computes the union of all the elements of this geometry. Heterogeneous GeometryCollections are fully supported.

The result obeys the following contract:

Unioning a set of LineStrings has the effect of fully noding and dissolving the linework.
Unioning a set of Polygons will always return a Polygonal geometry (unlike {link #union(Geometry)}, which may return geometrys of lower dimension if a topology collapse occurred.
-}
union :: Geometry a -> Geometry b -> Some Geometry
union = geo_2 R.union


unaryUnion :: Geometry a -> Some Geometry
unaryUnion = geo_1 R.unaryUnion

-- | Computes and returns a Point guaranteed to be on the interior of this geometry.
pointOnSurface :: Geometry a -> Geometry Point
pointOnSurface g = ensurePoint $ geo_1 R.pointOnSurface g

-- | Returns a Point object representing the geometric center of the geometry. The point is not guaranteed to be on the interior of the geometry.
centroid :: Geometry a -> Geometry Point
centroid g = ensurePoint $ geo_1 R.centroid g

node :: Geometry a -> Some Geometry
node = geo_1 R.node

-- | Return a Delaunay triangulation of the vertex of the given geometry @g@, where @tol@ is  the snapping tolerance to use.
delaunayTriangulation ::  Geometry a -> Double -> Geometry MultiLineString
delaunayTriangulation g d = ensureMultiLineString (geo_1 (`R.delaunayTriangulation` d) g)

-- | Returns the Voronoi polygons of a set of Vertices given as input
voronoiDiagram :: Geometry a 
               -> Maybe (Geometry b)
               -> Double 
               -> Bool 
               -> Some Geometry
voronoiDiagram g menv tol onlyEdges = runGeos $ do
  g':: RG.GeomConst <- convertGeometryToRaw g
  env' <- traverse convertGeometryToRaw menv
  convertGeometryFromRaw =<< R.voronoiDiagram g' env' tol onlyEdges
