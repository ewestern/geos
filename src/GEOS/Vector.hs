{-# LANGUAGE TypeFamilies #-}
module GEOS.Vector where

import GEOS.Types 
import Data.VectorSpace
import Data.AdditiveGroup
import Data.AffineSpace
{-import qualified Data.Vector as V-}


instance AdditiveGroup Coordinate where
  zeroV = Coordinate2 zeroV zeroV
  (Coordinate2 x1 y1) ^+^ (Coordinate2 x2 y2) = Coordinate2 (x1 + x2) (y1 + y2)
  negateV (Coordinate2 x y) = Coordinate2 (-x) (-y) 

instance AffineSpace Coordinate where
  type Diff Coordinate = (Double, Double)
  (Coordinate2 x1 y1) .-. (Coordinate2 x2 y2) = (x1 .-. x2, y1 .-. y2)
  (Coordinate2 x1 y1) .+^ (x2, y2) = Coordinate2 (x1 .-. x2) (y1 .-. y2)

instance VectorSpace Coordinate where
  type Scalar Coordinate = Double
  d *^ (Coordinate2 x y) = Coordinate2 (x * d) (y * d)

instance InnerSpace Coordinate where
  (Coordinate2 x y) <.> (Coordinate2 x2 y2) = (x * x2) + (y * y2)


