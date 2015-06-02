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
          -> Geometry
          -> Geometry   
geo_1 f g = runGeos $ convertGeometryToRaw g >>= convertGeometryFromRaw 

geo_2 :: (RG.Geometry -> RG.Geometry -> Geos RG.Geometry)
          -> Geometry
          -> Geometry
          -> Geometry
geo_2 f g1 g2 = runGeos $ do
  g1' <- convertGeometryToRaw g1
  g2' <- convertGeometryToRaw g2
  convertGeometryFromRaw =<< f g1' g2'

envelope :: Geometry -> Geometry
envelope = geo_1 R.envelope 

intersection :: Geometry -> Geometry -> Geometry
intersection = geo_2 R.intersection

convexHull :: Geometry -> Geometry
convexHull = geo_1 R.convexHull

difference :: Geometry -> Geometry -> Geometry
difference = geo_2 R.difference

symmetricDifference :: Geometry -> Geometry -> Geometry
symmetricDifference = geo_2 R.symmetricDifference

boundary :: Geometry -> Geometry
boundary = geo_1 R.boundary

union :: Geometry -> Geometry -> Geometry
union = geo_2 R.union

unaryUnion :: Geometry -> Geometry 
unaryUnion = geo_1 R.unaryUnion

pointOnSurface :: Geometry -> Geometry
pointOnSurface = geo_1 R.pointOnSurface

getCentroid :: Geometry -> Geometry
getCentroid = geo_1 R.getCentroid

node :: Geometry -> Geometry
node = geo_1 R.node
 
delaunayTriangulation ::  Geometry -> Double -> Bool -> Geometry
delaunayTriangulation g d b = geo_1 (\ g' -> R.delaunayTriangulation g' d b) g

