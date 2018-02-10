{-# LANGUAGE ScopedTypeVariables #-}


module Data.Geometry.Geos.STRTree where

import Prelude hiding (foldr)
import qualified Data.Geometry.Geos.Raw.STRTree as RT
import qualified Data.Geometry.Geos.Raw.Geometry as RG
import Data.Geometry.Geos.Types
import Data.Geometry.Geos.Geometry
import Data.Geometry.Geos.Raw.Base
import Foreign
import qualified Data.Vector as V

-- can't make instance of Foldable because of Storable constraint
foldr :: (RT.STRTreeLike t, Storable a) => (a -> b -> b) -> b -> t a -> b
foldr f a = runGeos . RT.foldr f a


-- unfortunately, the api exposed by geos does not allow retrieval of original geometries
toList :: Storable a => RT.STRTree a -> [a]
toList = foldr (:) []

toVector :: Storable a => RT.STRTree a -> V.Vector a
toVector = foldr V.cons V.empty

-- would like to expose 'empty' as is common for haskell collections, but when initializing an STRTree we have to know the node size before hand

fromList :: Storable b => [(Geometry a, b)] -> RT.STRTree b
fromList = fromFoldable

empty :: RT.STRTreeBuilder a
empty = runGeos $ RT.createSTRTreeBuilder 10

build :: RT.STRTreeBuilder a -> RT.STRTree a
build = runGeos . RT.build 

insert :: Storable a => Geometry b -> a -> RT.STRTreeBuilder a -> ()
insert geom item tree = runGeos $ do
    rg :: RG.GeomConst <- convertGeometryToRaw geom
    RT.insert tree rg item
    return ()

{-|
`fromFoldable` creates an STRTree with a default node capacity of 10. For finer-grained control over the node capacity, `fromFoldable_` accepts a node-capacity argument.
-}
fromFoldable :: (Foldable f, Storable b) => f (Geometry a, b) -> RT.STRTree b
fromFoldable  = fromFoldable_ 10

fromFoldable_ :: (Foldable f, Storable b) => Int -> f (Geometry a, b) -> RT.STRTree b
fromFoldable_ capacity things = runGeos $ do
  tree <- RT.createSTRTreeBuilder capacity
  mapM_ (ins tree) things
  RT.build tree
  where ins tree' (g,b) = do
          rg :: RG.GeomConst <- convertGeometryToRaw g
          RT.insert tree' rg b


lookup :: Storable b => Geometry a -> RT.STRTree b -> V.Vector b
lookup g tree = runGeos $ do
  rg :: RG.GeomConst <- convertGeometryToRaw g
  RT.query tree rg
