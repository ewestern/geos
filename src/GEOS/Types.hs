module GEOS.Types where
import qualified Data.Vector as V
import Data.Monoid
import Data.Data

type SRID = Maybe Int

data Some :: (* -> *) -> * where
  Some :: f a -> Some f

data Geometry a where
  PointGeometry :: Point -> SRID -> Geometry Point
  LineStringGeometry :: LineString -> SRID -> Geometry LineString
  PolygonGeometry :: Polygon -> SRID -> Geometry LineString
  MultiPointGeometry :: MultiPoint -> SRID -> Geometry LineString
  MultiLineStringGeometry :: MultiLineString-> SRID -> Geometry LineString
  MultiPolygonGeometry :: MultiPolygon-> SRID -> Geometry LineString

withSomeGeometry :: Some Geometry -> (forall a . Geometry a -> b) -> b
withSomeGeometry (Some p) f = f p 

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
