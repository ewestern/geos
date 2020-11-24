{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Data.Geometry.Geos.STRTree
Maintainer  : pfrance@gmail.com

A query-only R-tree created using the Sort-Tile-Recursive (STR) algorithm. For two-dimensional spatial data.

The STR packed R-tree is simple to implement and maximizes space utilization; that is, as many leaves as possible are filled to capacity. Overlap between nodes is far less than in a basic R-tree. However, once the tree has been built (explicitly or on the first call to query), items may not be added or removed.

Described in: P. Rigaux, Michel Scholl and Agnes Voisard. Spatial Databases With Application To GIS. Morgan Kaufmann, San Francisco, 2002.

-}


module Data.Geometry.Geos.STRTree
  ( foldr
  , toList
  , toVector
  , fromList
  , unfoldr_
  , unfoldr
  , fromFoldable
  , fromFoldable_
  , lookup
  , RT.STRTree
  )
where

import           Prelude                 hiding ( foldr
                                                , lookup
                                                )
import qualified Data.Geometry.Geos.Raw.STRTree
                                               as RT
import qualified Data.Geometry.Geos.Raw.Geometry
                                               as RG
import           Data.Geometry.Geos.Geometry
import           Data.Geometry.Geos.Raw.Base
import           Foreign
import qualified Data.Vector                   as V

-- can't make instance of Foldable because of Storable constraint
foldr :: (Storable a) => (a -> b -> b) -> b -> RT.STRTree a -> b
foldr f a = runGeos . RT.foldr f a


-- unfortunately, the api exposed by geos does not allow retrieval of original geometries
toList :: Storable a => RT.STRTree a -> [a]
toList = foldr (:) []

toVector :: Storable a => RT.STRTree a -> V.Vector a
toVector = foldr V.cons V.empty

fromList :: Storable b => [(Geometry a, b)] -> RT.STRTree b
fromList = fromFoldable

unfoldr :: Storable a => (b -> Maybe ((Geometry g, a), b)) -> b -> RT.STRTree a
unfoldr = unfoldr_ 10

unfoldr_ :: Storable a => Int -> (b -> Maybe ((Geometry g, a), b)) -> b -> RT.STRTree a
unfoldr_ capacity func initial = runGeos $ do
  builder' <- RT.createSTRTreeBuilder capacity
  go builder' func initial
  where
    go builder f init' =
      case f init' of
        Just ((geom, item), next) -> do
          rg :: RG.GeomConst <- convertGeometryToRaw geom
          RT.insert builder rg item
          go builder f next
        Nothing -> do
          tree <- RT.build builder
          pure tree

{-|
`fromFoldable` creates an STRTree with a default node capacity of 10. For finer-grained control over the node capacity, `fromFoldable_` accepts a node-capacity argument.
-}
fromFoldable :: (Foldable f, Storable b) => f (Geometry a, b) -> RT.STRTree b
fromFoldable = fromFoldable_ 10

fromFoldable_
  :: (Foldable f, Storable b) => Int -> f (Geometry a, b) -> RT.STRTree b
fromFoldable_ capacity things = runGeos $ do
  tree <- RT.createSTRTreeBuilder capacity
  mapM_ (ins tree) things
  RT.build tree
 where
  ins tree' (g, b) = do
    rg :: RG.GeomConst <- convertGeometryToRaw g
    RT.insert tree' rg b


{-| 
Queries the index for all items whose extents intersect the given search Envelope.
Note that some kinds of indexes may also return objects which do not in fact intersect the query envelope.
-}
lookup :: Storable b => Geometry a -> RT.STRTree b -> V.Vector b
lookup g tree = runGeos $ do
  rg :: RG.GeomConst <- convertGeometryToRaw g
  RT.query tree rg
