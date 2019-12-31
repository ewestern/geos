module Data.Geometry.Geos.Relatable (
  Relatable(..)
) where

import           Data.Geometry.Geos.Geometry
import           Data.Geometry.Geos.Prepared
import qualified Data.Geometry.Geos.Raw.Geometry as R
import qualified Data.Geometry.Geos.Raw.Prepared as RP


class Relatable a where
  contains ::  a -> Geometry b -> Bool
  coveredBy :: a -> Geometry b -> Bool
  covers :: a -> Geometry b -> Bool
  crosses :: a -> Geometry b -> Bool
  disjoint ::  a -> Geometry b -> Bool
  intersects :: a -> Geometry b -> Bool
  overlaps :: a -> Geometry b -> Bool
  touches ::  a -> Geometry b -> Bool
  within ::  a -> Geometry b -> Bool


instance Relatable (Geometry a) where
  disjoint   = binaryPredicate R.disjoint
  touches    = binaryPredicate R.touches
  intersects = binaryPredicate R.intersects
  contains   = binaryPredicate R.contains
  within     = binaryPredicate R.within
  crosses    = binaryPredicate R.crosses
  overlaps   = binaryPredicate R.overlaps
  covers     = binaryPredicate R.covers
  coveredBy  = binaryPredicate R.coveredBy


instance Relatable RP.PreparedGeometry where
  contains   = queryPrepared RP.contains
  coveredBy  = queryPrepared RP.coveredBy
  covers     = queryPrepared RP.covers
  crosses    = queryPrepared RP.crosses
  disjoint   = queryPrepared RP.disjoint
  intersects = queryPrepared RP.intersects
  overlaps   = queryPrepared RP.overlaps
  touches    = queryPrepared RP.touches
  within     = queryPrepared RP.within

