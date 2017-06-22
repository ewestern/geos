{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Geos.Geometry (
    convertGeometryFromRaw
  , convertGeometryToRaw
  , convertMultiPolygonFromRaw
  , ensurePoint
  , ensureLineString
  , ensureLinearRing
  , ensurePolygon
  , ensureMultiPoint
  , ensureMultiPolygon
  , ensureMultiLineString
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

import Data.Geometry.Geos.Types
import qualified Data.Vector as V
import qualified Data.Geometry.Geos.Raw.Geometry as R
import qualified Data.Geometry.Geos.Raw.CoordSeq as RC
import Data.Geometry.Geos.Raw.Base
import Control.Monad

-- | Returns the distance from the origin of LineString to the point projected on the geometry (that is to a point of the line the closest to the given point).
project :: Geometry LineString -> Geometry Point -> Double
project g1 g2 = runGeos $ do
  g1' :: R.Geom <- convertGeometryToRaw g1
  g2' :: R.Geom <- convertGeometryToRaw g2
  R.project g1' g2'

-- | Like @project@, but returns the distance as a Double between 0 and 1.
projectNormalized :: Geometry Point -> Geometry LineString -> Double
projectNormalized g1 g2 = runGeos $ do
  g1' :: R.Geom <- convertGeometryToRaw g1
  g2' :: R.Geom <- convertGeometryToRaw g2
  R.project g1' g2'

-- | Given a distance, returns the point (or closest point) within the geometry LineString that distance.
interpolate :: Geometry LineString -> Double -> Geometry Point
interpolate g d = runGeos $ do
  g' :: R.Geom <- convertGeometryToRaw  g
  s <- R.getSRID g'
  p <- convertPointFromRaw =<< (R.interpolate g' $ realToFrac d)
  return $ PointGeometry p s


-- | Like @interpolate@, but takes the distance as a double between 0 and 1.
interpolateNormalized :: Geometry LineString -> Double -> Geometry Point
interpolateNormalized g d = runGeos $ do
  g' :: R.Geom <- convertGeometryToRaw g
  s <- R.getSRID g'
  p <- convertPointFromRaw =<<  (R.interpolateNormalized g' $ realToFrac d)
  return $ PointGeometry p s

binaryPredicate_ :: (R.GeomConst -> R.GeomConst -> Geos Bool)
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

-- | Returns True if the DE-9IM intersection matrix for the two geometries is T*F**FFF*.
equals :: Geometry a -> Geometry a -> Bool
equals = binaryPredicate_ R.equals

-- | Returns True if the two geometries are exactly equal, up to a specified tolerance. The tolerance value should be a floating point number representing the error tolerance in the comparison, e.g., @equalsExact g1 g2 0.001 @  will compare equality to within one thousandth of a unit.
equalsExact :: Geometry a -> Geometry a -> Double -> Bool
equalsExact g1 g2 d = binaryPredicate_ (\g1' g2' -> R.equalsExact g1' g2' d) g1 g2

convertGeometryToRaw :: (R.Geometry a, R.CoordSeqInput a ~ cb, RC.CoordinateSequence cb) => Geometry b -> Geos a
convertGeometryToRaw = \case
    PointGeometry pg s -> convertPointToRaw pg s
    LineStringGeometry lsg s -> convertLineStringToRaw lsg s
    LinearRingGeometry lg s -> convertLinearRingToRaw lg s
    PolygonGeometry pg s -> convertPolygonToRaw pg s
    MultiPointGeometry mp s -> convertMultiPointToRaw mp s
    MultiLineStringGeometry ml s -> convertMultiLineStringToRaw ml s
    MultiPolygonGeometry mp s -> convertMultiPolygonToRaw mp s

convertPointToRaw :: R.Geometry b => Point -> Maybe Int -> Geos b
convertPointToRaw (Point c) s = do
  cs <- RC.createEmptyCoordinateSequence 1 (dimensionsCoordinate c)
  setCoordinateSequence cs 0 c
  R.createPoint cs >>= R.setSRID s


convertLinearRingToRaw :: R.Geometry b => LinearRing -> SRID -> Geos b
convertLinearRingToRaw (LinearRing cs) s = do
  csr <- RC.createEmptyCoordinateSequence len (dimensionsCoordinateSequence cs)
  V.zipWithM_ (setCoordinateSequence csr) (V.enumFromN 0 len) cs
  R.createLinearRing csr >>= R.setSRID s
  where
    len = V.length cs

convertLineStringToRaw :: R.Geometry b => LineString -> SRID -> Geos b
convertLineStringToRaw (LineString cs) s = do
  csr <- RC.createEmptyCoordinateSequence len (dimensionsCoordinateSequence cs)
  V.zipWithM_ (setCoordinateSequence csr) ( V.enumFromN 0 len) cs
  R.createLineString csr >>=  R.setSRID s
  where
    len = V.length cs

convertPolygonToRaw :: R.Geometry a => Polygon -> SRID -> Geos a
convertPolygonToRaw (Polygon lrs) s = do
  ext :: R.GeomConst <- convertLinearRingToRaw (V.head lrs) s
  inn :: V.Vector R.GeomConst <- (flip convertLinearRingToRaw $ s) `V.mapM` V.tail lrs
  R.createPolygon ext (V.toList inn)  >>= R.setSRID s

convertMultiPointToRaw :: R.Geometry a => MultiPoint -> SRID -> Geos a
convertMultiPointToRaw (MultiPoint vp) s = do
  vr <- (flip convertPointToRaw $ s) `V.mapM` vp
  R.createMultiPoint (V.toList vr) >>= R.setSRID s

convertMultiLineStringToRaw :: R.Geometry a => MultiLineString -> SRID -> Geos a
convertMultiLineStringToRaw (MultiLineString vl) s = do
  vr <- (flip convertLineStringToRaw $ s) `V.mapM` vl
  R.createMultiLineString (V.toList vr) >>= R.setSRID s

convertMultiPolygonToRaw :: R.Geometry a => MultiPolygon -> SRID -> Geos a
convertMultiPolygonToRaw (MultiPolygon vp) s = do
  vr <- (flip convertPolygonToRaw $ s) `V.mapM` vp
  R.createMultiPolygon (V.toList vr) >>= R.setSRID s

setCoordinateSequence :: RC.CoordinateSequence a => a -> Int -> Coordinate -> Geos ()
setCoordinateSequence cs i (Coordinate2 x y) =
  RC.setCoordinateSequenceX cs i x >> RC.setCoordinateSequenceY cs i y

setCoordinateSequence cs i (Coordinate3 x y z) =
  RC.setCoordinateSequenceX cs i x >> RC.setCoordinateSequenceY cs i y >> RC.setCoordinateSequenceZ cs i z

--- Conversions
--
convertGeometryFromRaw :: (R.Geometry a, R.CoordSeqInput a ~ cb, RC.CoordinateSequence cb) => a -> Geos (Some Geometry)
convertGeometryFromRaw rg = do
    s <- R.getSRID rg
    tid <- R.getTypeId rg
    case tid of
        R.PointTypeId -> do
          p <- convertPointFromRaw rg
          return $ Some $ (PointGeometry p s)
        R.LineStringTypeId -> do
          l <- convertLineStringFromRaw rg
          return $ Some (LineStringGeometry l s)
        R.LinearRingTypeId -> do
           l <- convertLinearRingFromRaw rg
           return $ Some (LinearRingGeometry l s)
        R.PolygonTypeId -> do
          p <- convertPolygonFromRaw rg
          return $ Some (PolygonGeometry p s)
        R.MultiPointTypeId -> do
          mp <- convertMultiPointFromRaw rg
          return $ Some (MultiPointGeometry mp s)
        R.MultiLineStringTypeId -> do
          ml <- convertMultiLineStringFromRaw rg
          return $ Some (MultiLineStringGeometry ml s)
        R.MultiPolygonTypeId -> do
          mp <- convertMultiPolygonFromRaw rg
          return $ Some (MultiPolygonGeometry mp s)
        R.GeometryCollectionTypeId -> error "GeometryCollection currently unsupported"

ensurePoint :: Some Geometry -> Geometry Point
ensurePoint g = withSomeGeometry g $ \g' -> case g' of
  PointGeometry _ _ -> g'
  _ -> error "This geometry was expected to be a Point"

ensureLineString :: Some Geometry -> Geometry LineString
ensureLineString g = withSomeGeometry g $ \g' -> case g' of
  LineStringGeometry _ _ -> g'
  _ -> error "This geometry was expected to be a LineString"

ensureLinearRing :: Some Geometry -> Geometry LinearRing
ensureLinearRing g = withSomeGeometry g $ \g' -> case g' of
  LinearRingGeometry _ _ -> g'
  _ -> error "This geometry was expected to be a LinearRing"

ensurePolygon :: Some Geometry -> Geometry Polygon
ensurePolygon g = withSomeGeometry g $ \g' -> case g' of
  PolygonGeometry _ _  -> g'
  _ -> error "This geometry was expected to be a Polygon"

ensureMultiPoint :: Some Geometry -> Geometry MultiPoint
ensureMultiPoint g = withSomeGeometry g $ \p' -> case p' of
  MultiPointGeometry _ _ -> p'
  _ -> error "This geometry was expected to be a MultiPoint"

ensureMultiLineString :: Some Geometry -> Geometry MultiLineString
ensureMultiLineString g = withSomeGeometry g $ \p' -> case p' of
  MultiLineStringGeometry _ _ -> p'
  _ -> error "This geometry was expected to be a MultiLineString"

ensureMultiPolygon :: Some Geometry -> Geometry MultiPolygon
ensureMultiPolygon g = withSomeGeometry g $ \p' -> case p' of
  MultiPolygonGeometry _ _ -> p'
  _ -> error "This geometry was expected to be a MultiPolygon"


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

convertPointFromRaw :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)  => a -> Geos Point
convertPointFromRaw g = do
  cs <- R.getCoordinateSequence g
  Point <$> getPosition cs 0


convertSequenceFromRaw ::  (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
                        =>  a -> Geos CoordinateSequence
convertSequenceFromRaw g = do
  cs  <- R.getCoordinateSequence g
  size <- R.getNumCoordinates g
  V.generateM size (getPosition cs)

convertLineStringFromRaw :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
                          => a -> Geos LineString
convertLineStringFromRaw g = LineString <$> convertSequenceFromRaw g

convertLinearRingFromRaw ::  (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
                          => a -> Geos LinearRing
convertLinearRingFromRaw g = LinearRing <$> convertSequenceFromRaw g

convertPolygonFromRaw :: R.Geometry a => a -> Geos Polygon
convertPolygonFromRaw g = do
  is <- R.getNumInteriorRings g
  ext :: R.GeomConst <- R.getExteriorRing g
  ins :: V.Vector R.GeomConst <- V.generateM is (R.getInteriorRingN g)
  Polygon <$> convertLinearRingFromRaw `V.mapM` (ext `V.cons` ins)

{-
Enforces using GeomConst for following functions

-}
getGeometryN  :: R.Geometry a => a -> Int -> Geos R.GeomConst
getGeometryN = R.getGeometryN

convertMultiPointFromRaw :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca) => a -> Geos MultiPoint
convertMultiPointFromRaw g = do
  ng <- R.getNumGeometries g
  MultiPoint <$> V.generateM ng (\i -> convertPointFromRaw =<< getGeometryN g i )

convertMultiLineStringFromRaw :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca) => a -> Geos MultiLineString
convertMultiLineStringFromRaw g = do
  ng <- R.getNumGeometries g
  MultiLineString <$> V.generateM ng (\i -> convertLineStringFromRaw =<< getGeometryN g i)

convertMultiPolygonFromRaw :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca) => a -> Geos MultiPolygon
convertMultiPolygonFromRaw g = do
  ng <- R.getNumGeometries g
  MultiPolygon <$> V.generateM ng (\i -> convertPolygonFromRaw =<< getGeometryN g i)

area  :: Geometry a -> Double
area g = runGeos $ do
  r :: R.Geom <- convertGeometryToRaw g
  d <- R.area r
  return d

-- | Returns the length of this geometry (e.g., 0 for a Point, the length of a LineString, or the circumference of a Polygon).
geometryLength :: Geometry a -> Double
geometryLength g = runGeos $ do
  r :: R.Geom <- convertGeometryToRaw g
  l <- R.geometryLength r
  return l

-- | NOTE: Data.Geometry.Geos distance calculations are linear – in other words, Data.Geometry.Geos does not perform a spherical calculation even if the SRID specifies a geographic coordinate system.
distance :: Geometry a -> Geometry a -> Double
distance p g = runGeos $ do
  p' :: R.Geom <- convertGeometryToRaw p
  g' :: R.Geom <- convertGeometryToRaw g
  R.distance p' g'

hausdorffDistance :: Geometry a -> Geometry a -> Double
hausdorffDistance p g = runGeos $ do
  p' :: R.Geom <- convertGeometryToRaw p
  g' :: R.Geom <- convertGeometryToRaw g
  R.hausdorffDistance p' g'

-- | Returns the closest points of the two geometries. The first point comes from g1 geometry and the second point comes from g2.
nearestPoints :: Geometry a -> Geometry a -> (Coordinate, Coordinate)
nearestPoints g1 g2 = runGeos $ do
  g1' :: R.Geom <- convertGeometryToRaw g1
  g2' :: R.Geom <- convertGeometryToRaw g2
  cs :: RC.CoordSeq <- R.nearestPoints g1' g2'
  p1 <- getPosition cs 0
  p2 <- getPosition cs 1
  return (p1, p2)
