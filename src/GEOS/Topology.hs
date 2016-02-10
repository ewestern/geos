{-# LANGUAGE CPP #-}
module GEOS.Topology (
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
import GEOS.Types
import GEOS.Raw.Base
import GEOS.Geometry
import qualified GEOS.Raw.Topology as R
import qualified GEOS.Raw.Geometry as RG


geo_1 :: ( RG.Geometry -> Geos RG.Geometry )
          -> Geometry a
          -> Some Geometry
geo_1 f g = runGeos $ do
    geo <- convertGeometryToRaw g 
    convertGeometryFromRaw =<< f geo

geo_2 :: (RG.Geometry -> RG.Geometry -> Geos RG.Geometry)
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

intersection :: Geometry a -> Geometry b -> Some Geometry
intersection = geo_2 R.intersection

-- | Returns the smallest Polygon that contains all the points in the geometry.
convexHull :: Geometry a -> Some Geometry
convexHull = geo_1 R.convexHull

difference :: Geometry a -> Geometry b -> Some Geometry
difference = geo_2 R.difference

symmetricDifference :: Geometry a -> Geometry b -> Some Geometry
symmetricDifference = geo_2 R.symmetricDifference

boundary :: Geometry a -> Some Geometry
boundary = geo_1 R.boundary

union :: Geometry a -> Geometry b -> Some Geometry
union = geo_2 R.union

unaryUnion :: Geometry a -> Some Geometry 
unaryUnion = geo_1 R.unaryUnion

-- | Computes and returns a Point guaranteed to be on the interior of this geometry.
pointOnSurface :: Geometry a -> Geometry Point
pointOnSurface g = withSomeGeometry  (geo_1 R.pointOnSurface $ g) $ \pg@(PointGeometry _ _) -> pg


-- | Returns a Point object representing the geometric center of the geometry. The point is not guaranteed to be on the interior of the geometry.
centroid :: Geometry a -> Some Geometry
centroid = geo_1 R.centroid

node :: Geometry a -> Some Geometry
node = geo_1 R.node
 
delaunayTriangulation ::  Geometry a -> Double -> Bool -> Some Geometry
delaunayTriangulation g d b = geo_1 (\ g' -> R.delaunayTriangulation g' d b) g

#if GEOS_VERSION_MAJOR > 3 && GEOS_VERSION_MINOR > 4
voronoiDiagram :: Geometry a  -> Geometry b -> Double -> Bool -> Some Geometry 
voronoiDiagram g env tol onlyEdges = runGeos $ do
  g' <- convertGeometryToRaw g 
  env' <- convertGeometryToRaw env
	convertGeometryFromRaw =<< R.voronoiDiagram g' env' tol onlyEndges

#endif
