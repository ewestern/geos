module Helpers where


import qualified Data.Vector as V

import Data.Geometry.Geos.Geometry

makePoint (c1, c2) = point $ coordinate2 c1 c2
makePointGeo c = PointGeometry (makePoint c) Nothing

makeLinearRing :: [(Double, Double)] -> Either GeometryConstructionError LinearRing
makeLinearRing = linearRing . V.map (uncurry coordinate2) . V.fromList

makeLinearRingGeo =  fmap (\lr -> LinearRingGeometry lr Nothing) . makeLinearRing

makeLineString :: [(Double, Double)] -> Either GeometryConstructionError LineString
makeLineString = lineString . V.map (uncurry coordinate2) . V.fromList

makeLineStringGeo cs = fmap (\ls -> LineStringGeometry ls Nothing) $ makeLineString cs 


makePolygon :: [[(Double, Double)]] -> Either GeometryConstructionError Polygon
makePolygon ls = do
  lRings <- traverse makeLinearRing ls
  polygon $ V.fromList lRings

makePolygonGeo cs = fmap (\pg -> PolygonGeometry pg Nothing) $ (makePolygon cs)

makeMultiLineString :: [[(Double, Double)]] -> Either GeometryConstructionError MultiLineString
makeMultiLineString ls = fmap (multiLineString . V.fromList) (traverse makeLineString ls)

makeMultiLineStringGeo lss = fmap (\ls -> MultiLineStringGeometry ls Nothing) $ makeMultiLineString lss

fromRight :: Either a b -> b
fromRight (Right v) = v
fromRight _         = error "Shouldn't happen."


