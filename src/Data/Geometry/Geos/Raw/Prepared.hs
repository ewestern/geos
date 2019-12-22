module Data.Geometry.Geos.Raw.Prepared
  ( PreparedGeometry(..)
  , withPreparedGeometry
  , prepare
  , contains
  , containsProperly
  , coveredBy
  , covers
  , crosses
  , disjoint
  , intersects
  , overlaps
  , touches
  , within
  )
where
import           Data.Geometry.Geos.Raw.Internal
import           Data.Geometry.Geos.Raw.Base
import qualified Data.Geometry.Geos.Raw.Geometry
                                               as RG
import           Foreign.Marshal.Utils
import           Foreign                 hiding ( throwIf )
import           Foreign.C.Types


newtype PreparedGeometry = PreparedGeometry {
  unPreparedGeometry :: ForeignPtr GEOSPreparedGeometry
} deriving (Show, Eq)

withPreparedGeometry
  :: PreparedGeometry -> (Ptr GEOSPreparedGeometry -> IO a) -> IO a
withPreparedGeometry (PreparedGeometry g) = withForeignPtr g

prepare :: RG.Geometry a => a -> Geos PreparedGeometry
prepare g = withGeos $ \h -> do
  g' <- RG.withGeometry g $ \gp -> geos_Prepare h gp
  fp <- newForeignPtrEnv geos_PreparedGeomDestroy h g'
  return $ PreparedGeometry fp

queryPrepared
  :: RG.Geometry a
  => (  GEOSContextHandle_t
     -> Ptr GEOSPreparedGeometry
     -> Ptr GEOSGeometry
     -> IO CChar
     )
  -> String
  -> PreparedGeometry
  -> a
  -> Geos Bool
queryPrepared f s pg g = do
  b <- throwIf (2 ==) (mkErrorMessage s) $ withGeos $ \h ->
    RG.withGeometry g $ \gp -> withPreparedGeometry pg $ flip (f h) gp
  return . toBool $ b

contains :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
contains = queryPrepared geos_PreparedContains "contains"

containsProperly :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
containsProperly =
  queryPrepared geos_PreparedContainsProperly "containsProperly"

coveredBy :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
coveredBy = queryPrepared geos_PreparedCoveredBy "coveredBy"

covers :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
covers = queryPrepared geos_PreparedCovers "covers"

crosses :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
crosses = queryPrepared geos_PreparedCrosses "crosses"

disjoint :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
disjoint = queryPrepared geos_PreparedDisjoint "disjoint"

intersects :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
intersects = queryPrepared geos_PreparedIntersects "intersects"

overlaps :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
overlaps = queryPrepared geos_PreparedOverlaps "overlaps"

touches :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
touches = queryPrepared geos_PreparedTouches "touches"

within :: RG.Geometry a => PreparedGeometry -> a -> Geos Bool
within = queryPrepared geos_PreparedWithin "within"
