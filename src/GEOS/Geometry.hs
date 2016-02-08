{-# LANGUAGE LambdaCase, ScopedTypeVariables #-} 
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , area
  , geometryLength
  , distance
  , hausdorffDistance
  , nearestPoints

) where

import GEOS.Types
import qualified Data.Vector as V
import qualified GEOS.Raw.Geometry as R
import qualified GEOS.Raw.CoordSeq as RC
import GEOS.Raw.Base
import Data.Monoid ((<>))
import Control.Monad

project :: Geometry Point -> Geometry LineString -> Double
project g1 g2 = runGeos $ do
  g1'<- convertGeometryToRaw g1
  g2'<- convertGeometryToRaw g2
  R.project g1' g2'  

projectNormalized :: Geometry Point -> Geometry LineString -> Double
projectNormalized g1 g2 = runGeos $ do 
  g1' <- convertGeometryToRaw g1
  g2' <- convertGeometryToRaw g2
  R.project g1' g2'  

interpolate :: Geometry LineString -> Double -> Geometry Point
interpolate g d = runGeos $ do 
  g' <- convertGeometryToRaw  g
  sg <- convertGeometryFromRaw =<< (R.interpolate g' $ realToFrac d)
  return $ withSomeGeometry sg $ \pg@(PointGeometry _ _) -> pg


interpolateNormalized :: Geometry LineString -> Double -> Some Geometry
interpolateNormalized g d = runGeos $ do
  g' <- convertGeometryToRaw g
  convertGeometryFromRaw =<<  (R.interpolateNormalized g' $ realToFrac d)

binaryPredicate_ :: (R.Geometry -> R.Geometry -> Geos Bool)
                    -> Geometry a
                    -> Geometry b
                    -> Bool
binaryPredicate_ f g1 g2 = runGeos . join $ (f <$> convertGeometryToRaw g1 <*> convertGeometryToRaw g2)

instance Geo (Geometry a) where
  disjoint = binaryPredicate_ R.disjoint
  touches = binaryPredicate_ R.touches
  intersects = binaryPredicate_ R.intersects
  contains = binaryPredicate_ R.contains
  within = binaryPredicate_ R.within
  crosses = binaryPredicate_ R.crosses
  overlaps = binaryPredicate_ R.overlaps
  covers = binaryPredicate_ R.covers
  coveredBy = binaryPredicate_ R.coveredBy

equals :: Geometry a -> Geometry a -> Bool
equals = binaryPredicate_ R.equals

-- | Returns True if the two geometries are exactly equal, up to a specified tolerance. The tolerance value should be a floating point number representing the error tolerance in the comparison, e.g., @equalsExact g1 g2 0.001 @  will compare equality to within one thousandth of a unit.
equalsExact :: Geometry a -> Geometry a -> Double -> Bool
equalsExact g1 g2 d = binaryPredicate_ (\g1' g2' -> R.equalsExact g1' g2' d) g1 g2

convertGeometryToRaw :: Geometry a -> Geos R.Geometry
convertGeometryToRaw = \case 
    PointGeometry pg s -> convertPointToRaw pg s 
    LineStringGeometry lsg s -> convertLineStringToRaw lsg s
    LinearRingGeometry lg s -> convertLinearRingToRaw lg s
    PolygonGeometry pg s -> convertPolygonToRaw pg s 
    MultiPointGeometry _ _ -> error "multipoint"
    MultiLineStringGeometry _ _ -> error "multilineString"
    MultiPolygonGeometry _ _ -> error "multipolygon"


convertPointToRaw :: Point -> SRID -> Geos R.Geometry
convertPointToRaw (Point c) s = do
  cs :: RC.CoordSeqConst <- RC.createCoordinateSequence 1 (dimensionsCoordinate c)
  setCoordinateSequence cs 0 c 
  R.createPoint cs >>< \g -> R.setSRID g s


convertLinearRingToRaw :: LinearRing -> SRID -> Geos R.Geometry
convertLinearRingToRaw (LinearRing cs) s = do
  csr :: RC.CoordSeqConst <- RC.createCoordinateSequence len (dimensionsCoordinateSequence cs) 
  V.zipWithM_ (setCoordinateSequence csr) (V.enumFromN 0 len) cs 
  R.createLinearRing csr >>< \g -> R.setSRID g s
  where
    len = V.length cs
  
convertLineStringToRaw :: LineString -> SRID -> Geos R.Geometry
convertLineStringToRaw (LineString cs) s = do
  csr :: RC.CoordSeqConst <- RC.createCoordinateSequence len (dimensionsCoordinateSequence cs) 
  V.zipWithM_ (setCoordinateSequence csr) ( V.enumFromN 0 len) cs 
  R.createLineString csr >>< \g -> R.setSRID g s
  where
    len = V.length cs    

convertPolygonToRaw :: Polygon -> SRID -> Geos R.Geometry
convertPolygonToRaw (Polygon lrs) s = do
  ext <- convertLinearRingToRaw (V.head lrs) s
  inn <- (\v -> convertLinearRingToRaw v s) `V.mapM` V.tail lrs
  R.createPolygon ext (V.toList inn)  >>< \g -> R.setSRID g s


setCoordinateSequence :: RC.CoordinateSequence a => a -> Int -> Coordinate -> Geos () 
setCoordinateSequence cs i (Coordinate2 x y) = 
  RC.setCoordinateSequenceX cs i x >> RC.setCoordinateSequenceY cs i y 

setCoordinateSequence cs i (Coordinate3 x y z) = 
  RC.setCoordinateSequenceX cs i x >> RC.setCoordinateSequenceY cs i y >> RC.setCoordinateSequenceZ cs i z 

--- Conversions
--
convertGeometryFromRaw :: R.Geometry -> Geos (Some Geometry)
convertGeometryFromRaw rg = do
    s <- R.getSRID rg
    tid <- R.getTypeId rg
    case tid of
        0 -> do
          p <- convertPointFromRaw rg
          return $ Some $ (PointGeometry p s)
        1 -> do
          l <- convertLineStringFromRaw rg
          return $ Some (LineStringGeometry l s)
        2 -> do
           l <- convertLinearRingFromRaw rg
           return $ Some (LinearRingGeometry l s)
        3 -> do
          p <- convertPolygonFromRaw rg
          return $ Some (PolygonGeometry p s)
        4 -> do
          mp <- convertMultiPointFromRaw rg 
          return $ Some (MultiPointGeometry mp s)
        5 -> do
          ml <- convertMultiLineStringFromRaw rg
          return $ Some (MultiLineStringGeometry ml s)
        6 -> do
          mp <- convertMultiPolygonFromRaw rg
          return $ Some (MultiPolygonGeometry mp s)
        e -> error $ "Unrecognized geometry type" <> show e 

getPosition :: RC.CoordinateSequence a => a -> Int -> Geos Coordinate 
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

area  :: Geometry a -> Double
area = runGeos . (convertGeometryToRaw >=> R.area)

geometryLength :: Geometry a -> Double
geometryLength = runGeos . (convertGeometryToRaw >=> R.geometryLength)

distance :: Geometry a -> Geometry a -> Double
distance p g = runGeos $ do
  p' <- convertGeometryToRaw p 
  g' <- convertGeometryToRaw g 
  R.distance p' g'

hausdorffDistance :: Geometry a -> Geometry a -> Double
hausdorffDistance p g = runGeos $ do
  p' <- convertGeometryToRaw p 
  g' <- convertGeometryToRaw g 
  R.hausdorffDistance p' g'

-- | Returns the closest points of the two geometries. The first point comes from g1 geometry and the second point comes from g2.

nearestPoints :: Geometry a -> Geometry a -> (Coordinate, Coordinate)
nearestPoints g1 g2 = runGeos $ do
  g1'<- convertGeometryToRaw g1
  g2'<- convertGeometryToRaw g2
  cs <- R.nearestPoints g1' g2'
  p1 <- getPosition cs 0
  p2 <- getPosition cs 1
  return (p1, p2)


