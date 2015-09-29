module GEOS.Raw.Prepared (
    PreparedGeometry (..)
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
) where
import GEOS.Raw.Internal 
import GEOS.Raw.Base
import qualified GEOS.Raw.Geometry as RG
import Foreign.Marshal.Utils
import Foreign
import Foreign.C.Types


newtype PreparedGeometry = PreparedGeometry {
  unPreparedGeometry :: ForeignPtr GEOSPreparedGeometry 
} deriving (Show, Eq)

withPreparedGeometry :: PreparedGeometry -> (Ptr GEOSPreparedGeometry -> IO a ) -> IO a
withPreparedGeometry (PreparedGeometry g) f = withForeignPtr g f

prepare :: RG.Geometry -> Geos PreparedGeometry
prepare g = withGeos $ \h -> do
    g' <- RG.withGeometry g $ \gp -> 
        geos_Prepare h gp
    fp <- newForeignPtrEnv geos_PreparedGeomDestroy h g'
    return $ PreparedGeometry fp

queryPrepared :: (GEOSContextHandle_t -> Ptr GEOSPreparedGeometry -> Ptr GEOSGeometry -> IO CChar)
                -> String
                -> PreparedGeometry
                -> RG.Geometry
                -> Geos Bool
queryPrepared f s pg g = withGeos $ \h -> do
  b <-  throwIf (\v -> v == 2) (mkErrorMessage s) $ 
          RG.withGeometry g $ \gp -> 
            withPreparedGeometry pg $ \pp ->
              f h pp gp
  return . toBool $ b

contains :: PreparedGeometry -> RG.Geometry -> Geos Bool
contains = queryPrepared geos_PreparedContains "contains"

containsProperly :: PreparedGeometry -> RG.Geometry -> Geos Bool
containsProperly = queryPrepared geos_PreparedContainsProperly "containsProperly"

coveredBy :: PreparedGeometry -> RG.Geometry -> Geos Bool
coveredBy = queryPrepared geos_PreparedCoveredBy "coveredBy"

covers :: PreparedGeometry -> RG.Geometry -> Geos Bool
covers = queryPrepared geos_PreparedCovers "covers"

crosses :: PreparedGeometry -> RG.Geometry -> Geos Bool
crosses = queryPrepared geos_PreparedCrosses "crosses"

disjoint :: PreparedGeometry -> RG.Geometry -> Geos Bool
disjoint = queryPrepared geos_PreparedDisjoint "disjoint"

intersects :: PreparedGeometry -> RG.Geometry -> Geos Bool
intersects = queryPrepared geos_PreparedIntersects "intersects"

overlaps :: PreparedGeometry -> RG.Geometry -> Geos Bool
overlaps = queryPrepared geos_PreparedOverlaps "overlaps"

touches :: PreparedGeometry -> RG.Geometry -> Geos Bool
touches = queryPrepared geos_PreparedTouches "touches"

within :: PreparedGeometry -> RG.Geometry -> Geos Bool
within = queryPrepared geos_PreparedWithin "within"
