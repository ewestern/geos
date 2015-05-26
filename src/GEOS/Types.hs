module GEOS.Types where
import qualified Data.Vector as V
import qualified GEOS.Raw.Geometry as R
import qualified GEOS.Raw.CoordSeq as RC
import GEOS.Raw.Base
import Data.Monoid ((<>))
import Control.Applicative ((<$>))

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
  | Coordinate3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double  deriving (Show, Eq)


type CoordinateSequence = V.Vector Coordinate 

newtype Point = Point {
  _unPoint :: Coordinate
} deriving (Show, Eq)
-- A LinearRing is a LineString that is closed
newtype LinearRing = LinearRing {
  _unLinearRing :: CoordinateSequence
} deriving (Show, Eq)

newtype LineString = LineString {
  _unLineString :: CoordinateSequence
} deriving (Show, Eq)

--  in a polygon, the fist LinearRing is the shell, and any following are holes.
newtype Polygon = Polygon {
  _unPolygon :: V.Vector LinearRing
} deriving (Show, Eq)

newtype MultiPoint = MultiPoint {
  _unMultiPoint :: V.Vector Point
} deriving (Show, Eq)

newtype MultiLineString = MultiLineString {
  _unMultiLineString :: V.Vector LineString
} deriving (Show, Eq)

newtype MultiPolygon = MultiPolygon {
  _unMultiPolygon :: V.Vector Polygon
} deriving (Show, Eq)


