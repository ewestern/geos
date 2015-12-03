module GEOS.Prepared (
    prepare
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

import qualified GEOS.Raw.Prepared as RP
import qualified GEOS.Raw.Geometry as RG
import GEOS.Raw.Base
import qualified GEOS.Geometry as G
import GEOS.Types
import Control.Monad


prepare :: Geometry a -> Geos RP.PreparedGeometry
prepare = G.convertGeometryToRaw >=> RP.prepare

queryPrepared :: (RP.PreparedGeometry -> RG.Geometry -> Geos Bool) 
                  -> RP.PreparedGeometry
                  -> Geometry a
                  -> Geos Bool 
queryPrepared f pg g = G.convertGeometryToRaw g >>= (f pg)

contains :: RP.PreparedGeometry -> Geometry a -> Geos Bool
contains = queryPrepared RP.contains

-- | The containsProperly predicate has the following equivalent definitions:

-- | Every point of the other geometry is a point of this geometry's interior. In other words, if the test geometry has any interaction with the boundary of the target geometry the result of containsProperly is false. This is different semantics to the @contains@ predicate, in which test geometries can intersect the target's boundary and still be contained.

-- | The advantage of using this predicate is that it can be computed efficiently, since it avoids the need to compute the full topological relationship of the input boundaries in cases where they intersect.

-- | An example use case is computing the intersections of a set of geometries with a large polygonal geometry. Since intersection is a fairly slow operation, it can be more efficient to use containsProperly to filter out test geometries which lie wholly inside the area. In these cases the intersection is known a priori to be exactly the original test geometry.

containsProperly :: RP.PreparedGeometry -> Geometry a -> Geos Bool
containsProperly = queryPrepared RP.containsProperly

coveredBy :: RP.PreparedGeometry -> Geometry a -> Geos Bool
coveredBy = queryPrepared RP.coveredBy 

covers :: RP.PreparedGeometry -> Geometry a -> Geos Bool
covers = queryPrepared RP.covers 

crosses :: RP.PreparedGeometry -> Geometry a -> Geos Bool
crosses = queryPrepared RP.crosses 

disjoint :: RP.PreparedGeometry -> Geometry a -> Geos Bool
disjoint = queryPrepared RP.disjoint 

intersects :: RP.PreparedGeometry -> Geometry a -> Geos Bool
intersects = queryPrepared RP.intersects 

overlaps :: RP.PreparedGeometry -> Geometry a -> Geos Bool
overlaps = queryPrepared RP.overlaps

touches :: RP.PreparedGeometry -> Geometry a -> Geos Bool
touches = queryPrepared RP.touches 

within :: RP.PreparedGeometry -> Geometry a -> Geos Bool
within = queryPrepared RP.within 


