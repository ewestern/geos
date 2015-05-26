module GEOS.Geometry where

import GEOS.Types
import qualified Data.Vector as V
import qualified GEOS.Raw.Geometry as R
import qualified GEOS.Raw.CoordSeq as RC
import GEOS.Raw.Base
import Data.Monoid ((<>))
import Control.Applicative ((<$>))



--- Conversions
--
convertGeometryFromRaw :: GEOSHandle -> R.Geometry -> Geometry
convertGeometryFromRaw h rg = let s = R.getSRID h rg
    in case R.getTypeId h rg of
          0 -> PointGeometry (convertPointFromRaw h rg ) s
          1 -> LineStringGeometry (convertLineStringFromRaw h rg) s 
          2 -> PolygonGeometry (convertPolygonFromRaw h rg) s
          3 -> MultiPointGeometry (convertMultiPointFromRaw h rg) s 
          4 -> MultiLineStringGeometry (convertMultiLineStringFromRaw h rg) s
          5 -> MultiPolygonGeometry (convertMultiPolygonFromRaw h rg) s
          e -> error $ "Unrecognized geometry type" <> show e 


    
-- todo, make more efficient by using the pointer functions instead of cloneing
getPosition :: GEOSHandle -> RC.CoordinateSequence -> Int -> Coordinate 
getPosition h cs i = 
    let dim = RC.getCoordinateSequenceDimensions h cs 
        x = RC.getCoordinateSequenceX h cs i   
        y = RC.getCoordinateSequenceY h cs i
        z = if dim == 3 
              then Just $ RC.getCoordinateSequenceZ h cs i
              else Nothing
    in case z of
      Nothing -> Coordinate2 x y
      Just z -> Coordinate3 x y z

convertPointFromRaw :: GEOSHandle -> R.Geometry -> Point
convertPointFromRaw h g = 
  let cs = R.getCoordinateSequence h g
  in Point $ getPosition h cs 0 

convertSequenceFromRaw :: GEOSHandle -> R.Geometry -> CoordinateSequence
convertSequenceFromRaw h g = 
  let cs = R.getCoordinateSequence h g
      size = R.getNumCoordinates h g
  in V.generate size (getPosition h cs)

convertLineStringFromRaw :: GEOSHandle -> R.Geometry -> LineString
convertLineStringFromRaw h g = LineString $ convertSequenceFromRaw h g

convertLinearRingFromRaw :: GEOSHandle -> R.Geometry -> LinearRing
convertLinearRingFromRaw h g = LinearRing $ convertSequenceFromRaw h g

convertPolygonFromRaw :: GEOSHandle -> R.Geometry -> Polygon
convertPolygonFromRaw h g = 
  let is = R.getNumInteriorRings h g
      ext = V.singleton $ R.getExteriorRing h g
      ins = V.generate is (R.getInteriorRingN h g)
  in Polygon $ (convertLinearRingFromRaw h) <$> (ext <> ins) 

convertMultiPointFromRaw :: GEOSHandle -> R.Geometry -> MultiPoint
convertMultiPointFromRaw h g =
  let ng = R.getNumGeometries h g
  in MultiPoint $ V.generate ng (\i -> convertPointFromRaw h $ R.getGeometryN h g i ) 
convertMultiLineStringFromRaw :: GEOSHandle -> R.Geometry -> MultiLineString
convertMultiLineStringFromRaw h g = 
  let ng = R.getNumGeometries h g
  in MultiLineString $ V.generate ng (\i -> convertLineStringFromRaw h $ R.getGeometryN h g i)

convertMultiPolygonFromRaw :: GEOSHandle -> R.Geometry -> MultiPolygon
convertMultiPolygonFromRaw h g = 
  let ng = R.getNumGeometries h g
  in MultiPolygon $ V.generate ng (\i -> convertPolygonFromRaw h $ R.getGeometryN h g i)
