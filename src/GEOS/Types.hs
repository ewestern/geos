{-# LANGUAGE RankNTypes, KindSignatures, GADTs, DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}

module GEOS.Types where
import qualified Data.Vector as V
import Data.Monoid
import Data.Data

class Geo a where
  contains ::  a -> Geometry b -> Bool
  coveredBy :: a -> Geometry b -> Bool
  covers :: a -> Geometry b -> Bool
  crosses :: a -> Geometry b -> Bool
  disjoint ::  a -> Geometry b -> Bool
  intersects :: a -> Geometry b -> Bool
  overlaps :: a -> Geometry b -> Bool
  touches ::  a -> Geometry b -> Bool
  within ::  a -> Geometry b -> Bool
  

type SRID = Maybe Int

data Some :: (* -> *) -> * where
  Some :: f a -> Some f

withSomeGeometry :: Some Geometry -> (forall a . Geometry a -> b) -> b
withSomeGeometry (Some p) f = f p 

instance Show (Some Geometry) where
  show (Some a) = "Some (" <> show a <> ")"

data Geometry a where
  PointGeometry :: Point -> SRID -> Geometry Point
  LineStringGeometry :: LineString -> SRID -> Geometry LineString
  PolygonGeometry :: Polygon -> SRID -> Geometry Polygon
  MultiPointGeometry :: MultiPoint -> SRID -> Geometry MultiPoint
  MultiLineStringGeometry :: MultiLineString -> SRID -> Geometry MultiLineString
  MultiPolygonGeometry :: MultiPolygon-> SRID -> Geometry MultiPolygon

deriving instance Eq (Geometry a)
deriving instance Show (Geometry a)

data Coordinate =
    Coordinate2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  | Coordinate3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double  deriving (Read, Ord, Show, Eq, Data, Typeable)


dimensionsCoordinate :: Coordinate -> Int
dimensionsCoordinate = length . gmapQ (const ())

type CoordinateSequence = V.Vector Coordinate 

dimensionsCoordinateSequence :: CoordinateSequence -> Int
dimensionsCoordinateSequence = dimensionsCoordinate . V.head

newtype Point = Point {
  unPoint :: Coordinate
} deriving (Read, Ord, Show, Eq, Data, Typeable)

-- A LinearRing is a LineString that is closed
newtype LinearRing = LinearRing {
  unLinearRing :: CoordinateSequence
} deriving (Read, Ord, Show, Eq, Data, Typeable)

instance Monoid LinearRing where
  mempty  = LinearRing V.empty
  mappend (LinearRing a) (LinearRing b) =  LinearRing (a <> b)

newtype LineString = LineString {
  unLineString :: CoordinateSequence
} deriving (Read, Ord, Show, Eq, Data, Typeable)

instance Monoid LineString where
  mempty  = LineString V.empty
  mappend (LineString a) (LineString b) =  LineString (a <> b)

-- | In a polygon, the fist LinearRing is the shell, and any following are holes.
newtype Polygon = Polygon {
  unPolygon :: V.Vector LinearRing
} deriving (Read, Ord, Show, Eq, Data, Typeable)

newtype MultiPoint = MultiPoint {
  unMultiPoint :: V.Vector Point
} deriving (Read, Ord, Show, Eq, Data, Typeable)

instance Monoid MultiPoint where
  mempty  = MultiPoint V.empty
  mappend (MultiPoint a) (MultiPoint b) =  MultiPoint (a <> b)

newtype MultiLineString = MultiLineString {
  unMultiLineString :: V.Vector LineString
} deriving (Read, Ord, Show, Eq, Data, Typeable)

newtype MultiPolygon = MultiPolygon {
  unMultiPolygon :: V.Vector Polygon
} deriving (Read, Ord, Show, Eq, Data, Typeable)
