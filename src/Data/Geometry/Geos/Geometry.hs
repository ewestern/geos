{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Geometry.Geos.Geometry
  ( Geometry(..)
  , GeometryConstructionError
  , Point(..)
  , point
  , LinearRing(..)
  , linearRing
  , LineString(..)
  , lineString
  , Polygon(..)
  , polygon
  , MultiPoint(..)
  , multiPoint
  , MultiLineString(..)
  , multiLineString
  , MultiPolygon(..)
  , multiPolygon
  , GeometryCollection
  , geometryCollection
  , Some(..)
  , Coordinate(..)
  , CoordinateSequence
  , coordinate2
  , coordinate3
  , SRID
  , binaryPredicate
  , convertGeometryFromRaw
  , convertGeometryToRaw
  , convertMultiPolygonFromRaw
  , ensurePoint
  , ensureLineString
  , ensureLinearRing
  , ensurePolygon
  , ensureMultiPoint
  , ensureMultiPolygon
  , ensureMultiLineString
  , ensureGeometryCollection
  , interpolate
  , interpolateNormalized
  , project
  , projectNormalized
  , equalsExact
  , equals
  , area
  , geometryLength
  , distance
  , hausdorffDistance
  , nearestPoints
  , withSomeGeometry
  , mapSomeGeometry
  )
where

import           Data.Data
import           Data.List                     (intercalate)
import           Data.Semigroup                as Sem
import qualified Data.Vector                   as V
import qualified Data.Geometry.Geos.Raw.Geometry
                                               as R
import qualified Data.Geometry.Geos.Raw.CoordSeq
                                               as RC
import           Data.Geometry.Geos.Raw.Base
import           Control.Monad
import           Control.Applicative            ( (<*>) )
import           GHC.Generics                   ( Generic )

-- | In all geometry types, SRID is used for compatability and is NOT used in calculations. For example, the `distance` between two PointGeometry with an SRID of `Just 4326` will return a distance between two points in Euclidean space in the units the PointGeometry is initialized with. It will not calculate the distance on a spheroid.
type SRID = Maybe Int

data Some :: (* -> *) -> * where
  Some ::f a -> Some f

instance Eq (Some Geometry) where
  (Some (PointGeometry p s)) == (Some (PointGeometry p2 s2)) = p == p2 && s == s2
  (Some (PointGeometry _ _)) == (Some _) = False
  (Some (LineStringGeometry g s)) == (Some (LineStringGeometry g2 s2)) = g == g2 && s == s2
  (Some (LineStringGeometry _ _)) == (Some _) = False
  (Some (PolygonGeometry g s)) == (Some (PolygonGeometry g2 s2)) = g == g2 && s == s2
  (Some (PolygonGeometry _ _)) == (Some _) = False
  (Some (MultiPointGeometry g s)) == (Some (MultiPointGeometry g2 s2)) = g == g2 && s == s2
  (Some (MultiPointGeometry _ _)) == (Some _) = False
  (Some (MultiLineStringGeometry g s)) == (Some (MultiLineStringGeometry g2 s2)) = g == g2 && s == s2
  (Some (MultiLineStringGeometry _ _)) == (Some _) = False

  (Some (MultiPolygonGeometry g s)) == (Some (MultiPolygonGeometry g2 s2)) = g == g2 && s == s2
  (Some (MultiPolygonGeometry _ _)) == (Some _) = False
  (Some (CollectionGeometry g s)) == (Some (CollectionGeometry g2 s2)) = g == g2 && s == s2
  (Some (CollectionGeometry _ _)) == (Some _) = False
  (Some (LinearRingGeometry g s)) == (Some (LinearRingGeometry g2 s2)) = g == g2 && s == s2
  (Some (LinearRingGeometry _ _)) == (Some _) = False

withSomeGeometry :: Some Geometry -> (forall a . Geometry a -> b) -> b
withSomeGeometry (Some p) f = f p

-- | the same as `withSomeGeometry` with its arguments reversed.
mapSomeGeometry :: (forall a . Geometry a -> b) -> Some Geometry -> b
mapSomeGeometry f (Some p) = f p


instance Show (Some Geometry) where
  show (Some a) = "Some (" <> show a <> ")"

data GeometryConstructionError
  = InvalidLinearRing
  | InvalidLineString
  | InvalidPolygon

data Geometry a where
  PointGeometry :: Point -> SRID -> Geometry Point
  LineStringGeometry :: LineString -> SRID -> Geometry LineString
  LinearRingGeometry :: LinearRing -> SRID -> Geometry LinearRing
  PolygonGeometry :: Polygon -> SRID -> Geometry Polygon
  MultiPointGeometry :: MultiPoint -> SRID -> Geometry MultiPoint
  MultiLineStringGeometry :: MultiLineString -> SRID -> Geometry MultiLineString
  MultiPolygonGeometry :: MultiPolygon -> SRID -> Geometry MultiPolygon
  CollectionGeometry :: GeometryCollection -> SRID -> Geometry GeometryCollection

deriving instance Eq (Geometry a)
deriving instance Show (Geometry a)

{-| Coordinate is the lightweight class used to store coordinates. Coordinate objects are two-dimensional points, with an additional z-ordinate. 
|-}
data Coordinate =
    Coordinate2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  | Coordinate3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Read, Ord, Eq, Data, Typeable, Generic)

instance Show Coordinate where
  show (Coordinate2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
  show (Coordinate3 x y z) =
    "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"


coordinate2 :: Double -> Double -> Coordinate
coordinate2  = Coordinate2

coordinate3 :: Double -> Double -> Double -> Coordinate
coordinate3 = Coordinate3

dimensionsCoordinate :: Coordinate -> Int
dimensionsCoordinate = length . gmapQ (const ())

type CoordinateSequence = V.Vector Coordinate

dimensionsCoordinateSequence :: CoordinateSequence -> Int
dimensionsCoordinateSequence = dimensionsCoordinate . V.head

newtype Point = Point Coordinate
 deriving (Read, Ord, Show, Eq, Data, Typeable, Generic)

point :: Coordinate -> Point
point = Point

-- A LinearRing is a LineString that is closed
newtype LinearRing = LinearRing {
  coordinateSequenceLinearRing :: CoordinateSequence } deriving (Read, Ord, Eq, Data, Typeable, Generic)

instance Show LinearRing where
  show (LinearRing vec) = 
    let inner = intercalate ", " $ show <$> V.toList vec
    in "[" ++ inner ++ "]"

linearRing :: CoordinateSequence -> Either GeometryConstructionError LinearRing
linearRing vec
  | 1 <= V.length vec && V.length vec < 4 = Left InvalidLinearRing
  | V.head vec /= V.last vec = Left InvalidLinearRing
  | otherwise = Right $ LinearRing vec

instance Sem.Semigroup LinearRing where
  (<>) (LinearRing a) (LinearRing b) = LinearRing (a <> b)

instance Monoid LinearRing where
  mempty = LinearRing V.empty

newtype LineString = LineString {
  coordinateSequenceLineString :: CoordinateSequence } deriving (Read, Ord, Eq, Data, Typeable, Generic)

instance Show LineString where
  show (LineString vec) = 
    let inner = intercalate ", " $ show <$> V.toList vec
    in "[" ++ inner ++ "]"

lineString :: CoordinateSequence -> Either GeometryConstructionError LineString
lineString  vec
  | V.length vec == 1 = Left InvalidLineString
  | V.length vec == 2 && V.last vec == V.head vec = Left InvalidLineString
  | otherwise = Right $ LineString vec

instance Sem.Semigroup LineString where
  (<>) (LineString a) (LineString b) = LineString (a <> b)

instance Monoid LineString where
  mempty = LineString V.empty

-- | In a polygon, the fist LinearRing is the shell, and any following are holes.
newtype Polygon = Polygon (V.Vector LinearRing)
 deriving (Read, Ord, Show, Eq, Data, Typeable, Generic)

polygon :: V.Vector LinearRing -> Either GeometryConstructionError Polygon
polygon vec = 
    let shell = maybe V.empty coordinateSequenceLinearRing $ vec V.!? 0
        holes = coordinateSequenceLinearRing <$> if V.null vec then V.empty else V.tail vec
    in 
      do
        _ <- if V.null shell && V.any (not . V.null) holes then Left InvalidPolygon else Right ()
        return . Polygon  $ V.cons (LinearRing shell) (fmap LinearRing holes)
  

newtype MultiPoint = MultiPoint (V.Vector Point)
 deriving (Read, Ord, Show, Eq, Data, Typeable, Generic)

multiPoint :: V.Vector Point -> MultiPoint
multiPoint = MultiPoint

instance Sem.Semigroup MultiPoint where
  (<>) (MultiPoint a) (MultiPoint b) = MultiPoint (a <> b)

instance Monoid MultiPoint where
  mempty = MultiPoint V.empty

newtype MultiLineString = MultiLineString (V.Vector LineString)
 deriving (Read, Ord, Show, Eq, Data, Typeable, Generic)

multiLineString :: V.Vector LineString -> MultiLineString
multiLineString = MultiLineString

newtype MultiPolygon = MultiPolygon (V.Vector Polygon)
 deriving (Read, Ord, Show, Eq, Data, Typeable, Generic)

multiPolygon :: V.Vector Polygon -> MultiPolygon
multiPolygon = MultiPolygon

newtype GeometryCollection = GeometryCollection (V.Vector (Some Geometry))
 deriving (Show, Eq, Typeable, Generic)

geometryCollection :: V.Vector (Some Geometry) -> GeometryCollection
geometryCollection  = GeometryCollection

-- | Returns the distance from the origin of LineString to the point projected on the geometry (that is to a point of the line the closest to the given point).
project :: Geometry LineString -> Geometry Point -> Double
project g1 g2 = runGeos $ do
  g1' :: R.Geom <- convertGeometryToRaw g1
  g2' :: R.Geom <- convertGeometryToRaw g2
  R.project g1' g2'

-- | Like @project@, but returns the distance as a Double between 0 and 1.
projectNormalized :: Geometry LineString -> Geometry Point -> Double
projectNormalized g1 g2 = runGeos $ do
  g1' :: R.Geom <- convertGeometryToRaw g1
  g2' :: R.Geom <- convertGeometryToRaw g2
  R.projectNormalized g1' g2'

-- | Given a distance, returns the point (or closest point) within the geometry LineString that distance.
interpolate :: Geometry LineString -> Double -> Geometry Point
interpolate g d = runGeos $ do
  g' :: R.Geom <- convertGeometryToRaw g
  s            <- R.getSRID g'
  p            <- convertPointFromRaw =<< R.interpolate g' ( realToFrac d)
  return $ PointGeometry p s


-- | Like @interpolate@, but takes the distance as a double between 0 and 1.
interpolateNormalized :: Geometry LineString -> Double -> Geometry Point
interpolateNormalized g d = runGeos $ do
  g' :: R.Geom <- convertGeometryToRaw g
  s <- R.getSRID g'
  p <- convertPointFromRaw =<< R.interpolateNormalized g' ( realToFrac d)
  return $ PointGeometry p s

binaryPredicate
  :: (R.GeomConst -> R.GeomConst -> Geos Bool)
  -> Geometry a
  -> Geometry b
  -> Bool
binaryPredicate f g1 g2 =
  runGeos . join $ (f <$> convertGeometryToRaw g1 <*> convertGeometryToRaw g2)

equals :: Geometry a -> Geometry a -> Bool
equals = binaryPredicate R.equals

-- | Returns True if the two geometries are exactly equal, up to a specified tolerance. The tolerance value should be a floating point number representing the error tolerance in the comparison, e.g., @equalsExact g1 g2 0.001 @  will compare equality to within one thousandth of a unit.
equalsExact :: Geometry a -> Geometry a -> Double -> Bool
equalsExact g1 g2 d =
  binaryPredicate (\g1' g2' -> R.equalsExact g1' g2' d) g1 g2

convertGeometryToRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ cb, RC.CoordinateSequence cb)
  => Geometry b
  -> Geos a
convertGeometryToRaw = \case
  PointGeometry           pg  s -> convertPointToRaw pg s
  LineStringGeometry      lsg s -> convertLineStringToRaw lsg s
  LinearRingGeometry      lg  s -> convertLinearRingToRaw lg s
  PolygonGeometry         pg  s -> convertPolygonToRaw pg s
  MultiPointGeometry      mp  s -> convertMultiPointToRaw mp s
  MultiLineStringGeometry ml  s -> convertMultiLineStringToRaw ml s
  MultiPolygonGeometry    mp  s -> convertMultiPolygonToRaw mp s
  CollectionGeometry      gc  s -> convertGeometryCollectionToRaw gc s

convertSomeGeometryToRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ cb, RC.CoordinateSequence cb)
  => Some Geometry
  -> Geos a
convertSomeGeometryToRaw sg = withSomeGeometry sg convertGeometryToRaw


convertPointToRaw :: R.Geometry b => Point -> Maybe Int -> Geos b
convertPointToRaw (Point c) s = do
  cs <- RC.createEmptyCoordinateSequence 1 (dimensionsCoordinate c)
  setCoordinateSequence cs 0 c
  R.createPoint cs >>= R.setSRID s


convertLinearRingToRaw :: R.Geometry b => LinearRing -> SRID -> Geos b
convertLinearRingToRaw =
  convertLineal coordinateSequenceLinearRing R.createLinearRing

convertLineStringToRaw :: R.Geometry b => LineString -> SRID -> Geos b
convertLineStringToRaw =
  convertLineal coordinateSequenceLineString R.createLineString
--
convertLineal
  :: R.Geometry b
  => (a -> CoordinateSequence)
  -> (RC.CoordSeqConst -> Geos b)
  -> a
  -> SRID
  -> Geos b
convertLineal getCoordSeq convertGeom geom srid = do
  let coordseq = getCoordSeq geom
      len      = V.length coordseq
  csr <- RC.createEmptyCoordinateSequence
    len
    (dimensionsCoordinateSequence coordseq)
  V.zipWithM_ (setCoordinateSequence csr) (V.enumFromN 0 len) coordseq
  convertGeom csr >>= R.setSRID srid

convertPolygonToRaw :: R.Geometry a => Polygon -> SRID -> Geos a
convertPolygonToRaw (Polygon lrs) s = do
  ext <- convertLinearRingToRaw (V.head lrs) s
  inn <- flip convertLinearRingToRaw s `V.mapM` V.tail lrs
  R.createPolygon ext (V.toList inn) >>= R.setSRID s

convertMultiPointToRaw :: R.Geometry a => MultiPoint -> SRID -> Geos a
convertMultiPointToRaw (MultiPoint vp) s = do
  vr <- flip convertPointToRaw s `V.mapM` vp
  R.createMultiPoint (V.toList vr) >>= R.setSRID s

convertMultiLineStringToRaw :: R.Geometry a => MultiLineString -> SRID -> Geos a
convertMultiLineStringToRaw (MultiLineString vl) s = do
  vr <- flip convertLineStringToRaw s `V.mapM` vl
  R.createMultiLineString (V.toList vr) >>= R.setSRID s

convertMultiPolygonToRaw :: R.Geometry a => MultiPolygon -> SRID -> Geos a
convertMultiPolygonToRaw (MultiPolygon vp) s = do
  vr <- flip convertPolygonToRaw s `V.mapM` vp
  R.createMultiPolygon (V.toList vr) >>= R.setSRID s

convertGeometryCollectionToRaw :: R.Geometry a => GeometryCollection -> SRID -> Geos a
convertGeometryCollectionToRaw (GeometryCollection vp) s = do
  vr <- convertSomeGeometryToRaw `V.mapM` vp
  R.createMultiPolygon (V.toList vr) >>= R.setSRID s




setCoordinateSequence
  :: RC.CoordinateSequence a => a -> Int -> Coordinate -> Geos ()
setCoordinateSequence cs i (Coordinate2 x y) =
  RC.setCoordinateSequenceX cs i x >> RC.setCoordinateSequenceY cs i y

setCoordinateSequence cs i (Coordinate3 x y z) =
  RC.setCoordinateSequenceX cs i x
    >> RC.setCoordinateSequenceY cs i y
    >> RC.setCoordinateSequenceZ cs i z

--- Conversions
--
convertGeometryFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ cb, RC.CoordinateSequence cb)
  => a
  -> Geos (Some Geometry)
convertGeometryFromRaw rg = do
  s   <- R.getSRID rg
  tid <- R.getTypeId rg
  case tid of
    R.PointTypeId -> do
      p <- convertPointFromRaw rg
      return $ Some (PointGeometry p s)
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
    R.GeometryCollectionTypeId -> do
      gc <- convertGeometryCollectionFromRaw rg
      return $ Some (CollectionGeometry gc s)


-- The following methods are useful when the type of a (Some Geometry) is known a priori
-- (i.e. the result of calling centroid is always a point)

ensurePoint :: Some Geometry -> Maybe (Geometry Point)
ensurePoint = mapSomeGeometry $ \case
  g@(PointGeometry _ _) -> Just g
  _                     -> Nothing

ensureLineString :: Some Geometry -> Maybe (Geometry LineString)
ensureLineString = mapSomeGeometry $ \case
  g@(LineStringGeometry _ _) -> Just g
  _                          -> Nothing

ensureLinearRing :: Some Geometry -> Maybe (Geometry LinearRing)
ensureLinearRing = mapSomeGeometry $ \case
  g@(LinearRingGeometry _ _) -> Just g
  _                          -> Nothing

ensurePolygon :: Some Geometry -> Maybe (Geometry Polygon)
ensurePolygon = mapSomeGeometry $ \case
  g@(PolygonGeometry _ _) -> Just g
  _                       -> Nothing

ensureMultiPoint :: Some Geometry -> Maybe (Geometry MultiPoint)
ensureMultiPoint = mapSomeGeometry $ \case
  g@(MultiPointGeometry _ _) -> Just g
  _                          -> Nothing

ensureMultiLineString :: Some Geometry -> Maybe (Geometry MultiLineString)
ensureMultiLineString = mapSomeGeometry $ \case
  g@(MultiLineStringGeometry _ _) -> Just g
  _                               -> Nothing

ensureMultiPolygon :: Some Geometry -> Maybe (Geometry MultiPolygon)
ensureMultiPolygon = mapSomeGeometry $ \case
  g@(MultiPolygonGeometry _ _) -> Just g
  _                            -> Nothing

ensureGeometryCollection :: Some Geometry -> Maybe (Geometry GeometryCollection)
ensureGeometryCollection = mapSomeGeometry $ \case
  g@(CollectionGeometry _ _) -> Just g
  _ -> Nothing

getPosition :: RC.CoordinateSequence a => a -> Int -> Geos Coordinate
getPosition cs i = do
  dim <- RC.getCoordinateSequenceDimensions cs
  x   <- RC.getCoordinateSequenceX cs i
  y   <- RC.getCoordinateSequenceY cs i
  z   <- if dim == 3
    then Just <$> RC.getCoordinateSequenceZ cs i
    else return Nothing
  case z of
    Nothing -> return $ Coordinate2 x y
    Just z' -> return $ Coordinate3 x y z'

convertPointFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos Point
convertPointFromRaw g = do
  cs <- R.getCoordinateSequence g
  Point <$> getPosition cs 0


convertSequenceFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos CoordinateSequence
convertSequenceFromRaw g = do
  cs   <- R.getCoordinateSequence g
  size <- R.getNumCoordinates g
  V.generateM size (getPosition cs)

convertLineStringFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos LineString
convertLineStringFromRaw g = LineString <$> convertSequenceFromRaw g

convertLinearRingFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos LinearRing
convertLinearRingFromRaw g = LinearRing <$> convertSequenceFromRaw g

convertPolygonFromRaw :: R.Geometry a => a -> Geos Polygon
convertPolygonFromRaw g = do
  is                          <- R.getNumInteriorRings g
  ext :: R.GeomConst          <- R.getExteriorRing g
  ins :: V.Vector R.GeomConst <- V.generateM is (R.getInteriorRingN g)
  Polygon <$> convertLinearRingFromRaw `V.mapM` (ext `V.cons` ins)

{-
Enforces using GeomConst for following functions

-}
getGeometryN :: R.Geometry a => a -> Int -> Geos R.GeomConst
getGeometryN = R.getGeometryN

convertMultiPointFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos MultiPoint
convertMultiPointFromRaw g = do
  ng <- R.getNumGeometries g
  MultiPoint <$> V.generateM ng (convertPointFromRaw <=< getGeometryN g)

convertMultiLineStringFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos MultiLineString
convertMultiLineStringFromRaw g = do
  ng <- R.getNumGeometries g
  MultiLineString
    <$> V.generateM ng (convertLineStringFromRaw <=< getGeometryN g)

convertMultiPolygonFromRaw
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos MultiPolygon
convertMultiPolygonFromRaw g = do
  ng <- R.getNumGeometries g
  MultiPolygon <$> V.generateM ng (convertPolygonFromRaw <=< getGeometryN g)


convertGeometryCollectionFromRaw 
  :: (R.Geometry a, R.CoordSeqInput a ~ ca, RC.CoordinateSequence ca)
  => a
  -> Geos GeometryCollection
convertGeometryCollectionFromRaw g = do
  ng <- R.getNumGeometries g
  GeometryCollection <$> V.generateM ng (convertGeometryFromRaw <=< getGeometryN g)

area :: Geometry a -> Double
area g = runGeos $ do
  r :: R.Geom <- convertGeometryToRaw g
  R.area r

-- | Returns the length of this geometry (e.g., 0 for a Point, the length of a LineString, or the circumference of a Polygon).
geometryLength :: Geometry a -> Double
geometryLength g = runGeos $ do
  r :: R.Geom <- convertGeometryToRaw g
  R.geometryLength r

-- | NOTE: @distance@ calculations are linear – in other words, @distance@ does not perform a spherical calculation even if the SRID specifies a geographic coordinate system.
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
  g1' :: R.Geom     <- convertGeometryToRaw g1
  g2' :: R.Geom     <- convertGeometryToRaw g2
  cs :: RC.CoordSeq <- R.nearestPoints g1' g2'
  p1                <- getPosition cs 0
  p2                <- getPosition cs 1
  return (p1, p2)


