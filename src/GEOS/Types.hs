module GEOS.Types where
import qualified Data.Vector as V
import Data.Monoid
import Data.Data

type SRID = Int
data Geometry = 
    PointGeometry Point SRID
  | LineStringGeometry LineString SRID
  | PolygonGeometry Polygon SRID
  | MultiPointGeometry MultiPoint SRID
  | MultiLineStringGeometry MultiLineString SRID
  | MultiPolygonGeometry MultiPolygon SRID deriving (Show, Eq)

data Coordinate =
    Coordinate2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  | Coordinate3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double  deriving (Show, Eq, Data, Typeable)


dimensionsCoordinate :: Coordinate -> Int
dimensionsCoordinate = length . gmapQ (const ())

type CoordinateSequence = V.Vector Coordinate 

dimensionsCoordinateSequence :: CoordinateSequence -> Int
dimensionsCoordinateSequence = dimensionsCoordinate . V.head

newtype Point = Point {
  unPoint :: Coordinate
} deriving (Show, Eq)

-- A LinearRing is a LineString that is closed
newtype LinearRing = LinearRing {
  unLinearRing :: CoordinateSequence
} deriving (Show, Eq)

instance Monoid LinearRing where
  mempty  = LinearRing V.empty
  mappend (LinearRing a) (LinearRing b) =  LinearRing (a <> b)

newtype LineString = LineString {
  unLineString :: CoordinateSequence
} deriving (Show, Eq)

instance Monoid LineString where
  mempty  = LineString V.empty
  mappend (LineString a) (LineString b) =  LineString (a <> b)

-- | In a polygon, the fist LinearRing is the shell, and any following are holes.
newtype Polygon = Polygon {
  unPolygon :: V.Vector LinearRing
} deriving (Show, Eq)

newtype MultiPoint = MultiPoint {
  unMultiPoint :: V.Vector Point
} deriving (Show, Eq)

instance Monoid MultiPoint where
  mempty  = MultiPoint V.empty
  mappend (MultiPoint a) (MultiPoint b) =  MultiPoint (a <> b)

newtype MultiLineString = MultiLineString {
  unMultiLineString :: V.Vector LineString
} deriving (Show, Eq)

newtype MultiPolygon = MultiPolygon {
  unMultiPolygon :: V.Vector Polygon
} deriving (Show, Eq)

