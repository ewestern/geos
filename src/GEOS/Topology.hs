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
  , getCentroid
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

envelope :: Geometry a -> Some Geometry
envelope = geo_1 R.envelope 

intersection :: Geometry a -> Geometry b -> Some Geometry
intersection = geo_2 R.intersection

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

-- todo: irrefutable pattern match?
pointOnSurface :: Geometry a -> Some Geometry
pointOnSurface = geo_1 R.pointOnSurface

getCentroid :: Geometry a -> Some Geometry
getCentroid = geo_1 R.getCentroid

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
