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
foldr :: Storable a => (a -> b -> b) -> b -> RT.STRTree a -> b
foldr f a = runGeos . RT.foldr f a


-- unfortunately, the api exposed by geos does not allow retrieval of original geometries
toList :: Storable a => RT.STRTree a -> [a]
toList = foldr (:) []

toVector :: Storable a => RT.STRTree a -> V.Vector a
toVector = foldr V.cons V.empty

-- would like to expose 'empty' as is common for haskell collections, but when initializing an STRTree we have to know the node size before hand

fromList :: Storable b => [(Geometry a, b)] -> RT.STRTree b
fromList = fromFoldable

--insert :: Storable a => Geometry b -> a -> RT.STRTree a -> RT.STRTree a
--insert geom item tree = runGeos $ do
--    rg :: RG.GeomConst <- convertGeometryToRaw geom
--    RT.insert tree rg item
--    return tree

fromFoldable :: (Foldable f, Storable b) => f (Geometry a, b) -> RT.STRTree b
fromFoldable things = runGeos $ do
  tree <- RT.createSTRTree $ max 4 $ length things
  mapM_ (ins tree) things
  return tree
  where ins tree' (g,b) = do
          rg :: RG.GeomConst <- convertGeometryToRaw g
          RT.insert tree' rg b

lookup :: Storable b => Geometry a -> RT.STRTree b -> V.Vector b
lookup g tree = runGeos $ do
  rg :: RG.GeomConst <- convertGeometryToRaw g
  RT.query tree rg
