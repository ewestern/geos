module GEOS.Types where
import qualified Data.Vector as V
import qualified GEOS.Raw.Geometry as R
import qualified GEOS.Raw.CoordSeq as RC
import GEOS.Raw.Base
import Data.Monoid ((<>))
import Control.Applicative ((<$>))

class Geo a where
  dimensions :: a -> Int


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


instance Geo Coordinate where
  dimensions (Coordinate2 _ _) = 2
  dimensions (Coordinate3 _ _ _) = 3

type CoordinateSequence = V.Vector Coordinate 

newtype Point = Point {
  unPoint :: Coordinate
} deriving (Show, Eq)

instance Geo Point where
  dimensions = dimensions . unPoint

-- A LinearRing is a LineString that is closed
newtype LinearRing = LinearRing {
  unLinearRing :: CoordinateSequence
} deriving (Show, Eq)

instance Geo LinearRing where
  dimensions = dimensions . V.head . unLinearRing

newtype LineString = LineString {
  unLineString :: CoordinateSequence
} deriving (Show, Eq)

instance Geo LineString where
  dimensions = dimensions . V.head . unLineString 

--  in a polygon, the fist LinearRing is the shell, and any following are holes.
newtype Polygon = Polygon {
  unPolygon :: V.Vector LinearRing
} deriving (Show, Eq)

instance Geo Polygon where
  dimensions = dimensions . V.head . unPolygon

newtype MultiPoint = MultiPoint {
  unMultiPoint :: V.Vector Point
} deriving (Show, Eq)

instance Geo MultiPoint where
  dimensions = dimensions . V.head . unMultiPoint

newtype MultiLineString = MultiLineString {
  unMultiLineString :: V.Vector LineString
} deriving (Show, Eq)

instance Geo MultiLineString where
  dimensions = dimensions . V.head . unMultiLineString

newtype MultiPolygon = MultiPolygon {
  unMultiPolygon :: V.Vector Polygon
} deriving (Show, Eq)

instance Geo MultiPolygon where
  dimensions = dimensions . V.head . unMultiPolygon

