{-# LANGUAGE ScopedTypeVariables #-}

module Data.Geometry.Geos.STRTree where

import qualified Data.Geometry.Geos.Raw.STRTree as RT
import qualified Data.Geometry.Geos.Raw.Geometry as RG
import Control.Monad
import Data.Geometry.Geos.Types
import Data.Geometry.Geos.Geometry
import Data.Geometry.Geos.Raw.Base
import Foreign
import qualified Data.Vector as V

createSTR :: (Foldable f, Storable b) => f (Geometry a, b) -> Geos (RT.STRTree b)
createSTR things = do
  tree <- RT.createSTRTree 10
  foldM ins tree things
  where ins tree' (g,b) = do
          rg :: RG.Geom <- convertGeometryToRaw g
          RT.insert tree' rg b

querySTR :: Storable b => Geos (RT.STRTree b) -> Geometry a -> V.Vector b
querySTR tree g = runGeos $ do
  rg :: RG.Geom <- convertGeometryToRaw g
  tree' <- tree
  RT.query tree' rg

-- query :: (Storable b, RG.Geometry a) => (STRTree b) -> a -> Geos (V.Vector b)
--  RP.prepare r
