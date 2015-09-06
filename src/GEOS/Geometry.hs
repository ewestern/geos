{-# LANGUAGE LambdaCase #-} 

module GEOS.Geometry (
  convertGeometryFromRaw
  , convertGeometryToRaw
  , interpolate
  , interpolateNormalized
  , project
  , projectNormalized
  , covers
  , coveredBy
  , equalsExact
  , equals
  , overlaps
  , contains
  , within
  , crosses
  , touches
  , disjoint
  , nearestPoints

) where

import GEOS.Types
import qualified Data.Vector as V
import qualified GEOS.Raw.Geometry as R
import qualified GEOS.Raw.CoordSeq as RC
import GEOS.Raw.Base
import Data.Monoid ((<>))
import Control.Applicative ((<$>))

project :: Geometry -> Geometry -> Double
project g1 g2 = runGeos $ do
  g1'<- convertGeometryToRaw g1
  g2'<- convertGeometryToRaw g2
  R.project g1' g2'  

projectNormalized :: Geometry -> Geometry -> Double
projectNormalized g1 g2 = runGeos $ do 
  g1' <-  convertGeometryToRaw g1
  g2' <- convertGeometryToRaw g2
  R.project g1' g2'  

interpolate :: Geometry -> Double -> Geometry 
interpolate g d = runGeos $ do 
  g' <- convertGeometryToRaw  g
  convertGeometryFromRaw =<< (R.interpolate g' $ realToFrac d)


interpolateNormalized :: Geometry -> Double -> Geometry 
interpolateNormalized g d = runGeos $ do
  g' <- convertGeometryToRaw g
  convertGeometryFromRaw =<<  (R.interpolateNormalized g' $ realToFrac d)

binaryPredicate_ :: (R.Geometry -> R.Geometry -> Geos Bool)
                    -> Geometry 
                    -> Geometry 
                    -> Bool
binaryPredicate_ f g1 g2 = runGeos $ do --(f <$> convertGeometryToRaw g1 <*> convertGeometryToRaw g2)
  g1' <- convertGeometryToRaw g1
  g2' <- convertGeometryToRaw g2
  f g1' g2'

disjoint :: Geometry -> Geometry -> Bool
disjoint = binaryPredicate_ R.disjoint

touches :: Geometry -> Geometry -> Bool
touches = binaryPredicate_ R.touches

crosses :: Geometry -> Geometry -> Bool
crosses = binaryPredicate_ R.crosses

within :: Geometry -> Geometry -> Bool
within = binaryPredicate_ R.within

contains :: Geometry -> Geometry -> Bool
contains = binaryPredicate_ R.contains

overlaps :: Geometry -> Geometry -> Bool
overlaps = binaryPredicate_ R.overlaps

equals :: Geometry -> Geometry -> Bool
equals = binaryPredicate_ R.equals

equalsExact :: Geometry -> Geometry -> Bool
equalsExact = binaryPredicate_ R.equalsExact

covers :: Geometry -> Geometry -> Bool
covers = binaryPredicate_ R.covers

coveredBy :: Geometry -> Geometry -> Bool
coveredBy = binaryPredicate_ R.coveredBy


convertGeometryToRaw :: Geometry -> Geos R.Geometry
convertGeometryToRaw = \case 
    PointGeometry pg s -> convertPointToRaw pg s 
    LineStringGeometry lsg s -> convertLineStringToRaw lsg s
    PolygonGeometry pg s -> convertPolygonToRaw pg s 
    MultiPointGeometry mp s -> error "multipoint"
    MultiLineStringGeometry mls s -> error "multilineString"
    MultiPolygonGeometry mps s -> error "multipolygon"


convertPointToRaw :: Point -> SRID -> Geos R.Geometry
convertPointToRaw p@(Point c) s = do
  cs <- RC.createCoordinateSequence 1 (dimensions p)
  setCoordinateSequence cs 1 c 
  R.createPoint cs >>< \g -> R.setSRID g s


convertLinearRingToRaw :: LinearRing -> SRID -> Geos R.Geometry
convertLinearRingToRaw l@(LinearRing cs) s = do
  csr <- RC.createCoordinateSequence len (dimensions l) 
  V.zipWithM_ (setCoordinateSequence csr) (V.enumFromN 0 len) cs 
  R.createLinearRing csr >>< \g -> R.setSRID g s
  where
    len = V.length cs
  
convertLineStringToRaw :: LineString -> SRID -> Geos R.Geometry
convertLineStringToRaw l@(LineString cs) s = do
  csr <- RC.createCoordinateSequence len (dimensions l) 
  V.zipWithM_ (setCoordinateSequence csr) ( V.enumFromN 0 len) cs 
  R.createLineString csr >>< \g -> R.setSRID g s
  where
    len = V.length cs    

convertPolygonToRaw :: Polygon -> SRID -> Geos R.Geometry
convertPolygonToRaw (Polygon lrs) s = do
  ext <- convertLinearRingToRaw (V.head lrs) s
  inn <- (\v -> convertLinearRingToRaw v s) `V.mapM` V.tail lrs
  R.createPolygon ext (V.toList inn) (V.length inn - 1)  >>< \g -> R.setSRID g s


setCoordinateSequence :: RC.CoordinateSequence -> Int -> Coordinate -> Geos () 
setCoordinateSequence cs i (Coordinate2 x y) = 
  RC.setCoordinateSequenceX cs i x >> RC.setCoordinateSequenceY cs i y 

setCoordinateSequence cs i (Coordinate3 x y z) = 
  RC.setCoordinateSequenceX cs i x >> RC.setCoordinateSequenceY cs i y >> RC.setCoordinateSequenceZ cs i z 

--- Conversions
--
convertGeometryFromRaw :: R.Geometry -> Geos Geometry
convertGeometryFromRaw rg = do
    s <- R.getSRID rg
    tid <- R.getTypeId rg
    ($s) <$> case tid of
        0 -> PointGeometry <$> (convertPointFromRaw rg )
        1 -> LineStringGeometry <$> (convertLineStringFromRaw rg) 
        2 -> PolygonGeometry <$> (convertPolygonFromRaw rg)
        3 -> MultiPointGeometry <$>  (convertMultiPointFromRaw rg) 
        4 -> MultiLineStringGeometry <$> (convertMultiLineStringFromRaw rg) 
        5 -> MultiPolygonGeometry <$> (convertMultiPolygonFromRaw rg)
        e -> error $ "Unrecognized geometry type" <> show e 

getPosition :: RC.CoordinateSequence -> Int -> Geos Coordinate 
getPosition cs i =  do 
    dim <- RC.getCoordinateSequenceDimensions cs 
    x <- RC.getCoordinateSequenceX cs i   
    y <- RC.getCoordinateSequenceY cs i
    z <- if dim == 3 
              then Just <$> RC.getCoordinateSequenceZ cs i
              else return Nothing
    case z of
      Nothing -> return $ Coordinate2 x y
      Just z' -> return $ Coordinate3 x y z'

convertPointFromRaw :: R.Geometry -> Geos Point
convertPointFromRaw g = do
  cs <- R.getCoordinateSequence g
  Point <$> getPosition cs 0 

convertSequenceFromRaw :: R.Geometry -> Geos CoordinateSequence
convertSequenceFromRaw g = do
  -- todo, consider using getCoordinateSequence_
  cs <- R.getCoordinateSequence g
  size <- R.getNumCoordinates g
  V.generateM size (getPosition cs)

convertLineStringFromRaw :: R.Geometry -> Geos LineString
convertLineStringFromRaw g = LineString <$> convertSequenceFromRaw g

convertLinearRingFromRaw :: R.Geometry -> Geos LinearRing
convertLinearRingFromRaw g = LinearRing <$> convertSequenceFromRaw g

convertPolygonFromRaw :: R.Geometry -> Geos Polygon
convertPolygonFromRaw g = do
  is <- R.getNumInteriorRings g
  ext <- V.singleton <$> R.getExteriorRing g
  ins <- V.generateM is (R.getInteriorRingN g)
  Polygon <$> convertLinearRingFromRaw `V.mapM` (ext <> ins) 

convertMultiPointFromRaw :: R.Geometry -> Geos MultiPoint
convertMultiPointFromRaw g = do
  ng <- R.getNumGeometries g
  MultiPoint <$> V.generateM ng (\i -> convertPointFromRaw =<< R.getGeometryN g i ) 

convertMultiLineStringFromRaw :: R.Geometry -> Geos MultiLineString
convertMultiLineStringFromRaw g = do
  ng <- R.getNumGeometries g
  MultiLineString <$> V.generateM ng (\i -> convertLineStringFromRaw =<< R.getGeometryN g i)

convertMultiPolygonFromRaw :: R.Geometry -> Geos MultiPolygon
convertMultiPolygonFromRaw g = do
  ng <- R.getNumGeometries g
  MultiPolygon <$> V.generateM ng (\i -> convertPolygonFromRaw =<< R.getGeometryN g i)

-- | Returns the closest points of the two geometries. The first point comes from g1 geometry and the second point comes from g2.
nearestPoints :: Geometry -> Geometry -> Geos (Coordinate, Coordinate)
nearestPoints g1 g2 = do
  g1'<- convertGeometryToRaw g1
  g2'<- convertGeometryToRaw g2
  cs <- R.nearestPoints g1' g2'
  p1 <- getPosition cs 0
  p2 <- getPosition cs 1
  return (p1, p2)


