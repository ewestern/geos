module Data.Geometry.Geos.Relatable where

import Data.Geometry.Geos.Geometry 
import qualified Data.Geometry.Geos.Raw.Geometry as R

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
  disjoint = binaryPredicate R.disjoint
  touches = binaryPredicate R.touches
  intersects = binaryPredicate R.intersects
  contains = binaryPredicate R.contains
  within = binaryPredicate R.within
  crosses = binaryPredicate R.crosses
  overlaps = binaryPredicate R.overlaps
  covers = binaryPredicate R.covers
  coveredBy = binaryPredicate R.coveredBy


