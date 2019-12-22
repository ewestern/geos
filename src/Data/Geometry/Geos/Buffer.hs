{-|
Module      : Data.Geometry.Geos.Raw.Buffer
Maintainer  : pfrance@gmail.com

Functions to compute the buffer of a geometry, for both positive and negative buffer distances.

In GIS, the positive (or negative) buffer of a geometry is defined as the Minkowski sum (or difference) of the geometry with a circle with radius equal to the absolute value of the buffer distance. In the CAD/CAM world buffers are known as offset curves. In morphological analysis the operation of positive and negative buffering is referred to as erosion and dilation.

The buffer operation always returns a polygonal result. The negative or zero-distance buffer of lines and points is always an empty Polygon.

Since true buffer curves may contain circular arcs, computed buffer polygons can only be approximations to the true geometry. The user can control the accuracy of the curve approximation by specifying the number of linear segments with which to approximate a curve.


-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Geometry.Geos.Buffer
  ( buffer
  , defaultBufferParams
  , BufferParams(..)
  , BufferJoinStyle(..)
  , BufferCapStyle(..)
  )
where
import qualified Data.Geometry.Geos.Raw.Buffer as R
import           Data.Geometry.Geos.Raw.Base
import           Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Geometry
                                               as RG

data BufferCapStyle =
  -- | the usual round end caps
    RoundCap
  -- | end caps are truncated flat at the line ends
  | SquareCap
  -- | end caps are squared off at the buffer distance beyond the line ends
  | FlatCap

data BufferJoinStyle =
    RoundJoin
  | MitreJoin
  | BevelJoin

convertCapStyle :: BufferCapStyle -> R.BufferCapStyle
convertCapStyle = \case
  RoundCap  -> R.capRound
  SquareCap -> R.capSquare
  FlatCap   -> R.capFlat

convertJoinStyle :: BufferJoinStyle -> R.BufferJoinStyle
convertJoinStyle = \case
  RoundJoin -> R.joinRound
  MitreJoin -> R.joinMitre
  BevelJoin -> R.joinBevel

data BufferParams = BufferParams {
    joinStyle :: BufferJoinStyle
  , capStyle :: BufferCapStyle
{-
Sets the limit on the mitre ratio used for very sharp corners.

The mitre ratio is the ratio of the distance from the corner to the end of the mitred offset corner. When two line segments meet at a sharp angle, a miter join will extend far beyond the original geometry. (and in the extreme case will be infinitely far.) To prevent unreasonable geometry, the mitre limit allows controlling the maximum length of the join corner. Corners with a ratio which exceed the limit will be beveled.
-}
  , mitreLimit :: Double
{- | 
Sets the number of line segments used to approximate an angle fillet.

* If quadrantSegments >= 1, joins are round, and quadrantSegments indicates the number of segments to use to approximate a quarter-circle.
* If quadrantSegments = 0, joins are bevelled (flat)
* If quadrantSegments < 0, joins are mitred, and the value of qs indicates the mitre ration limit as: mitreLimit = |quadrantSegments|
   
For round joins, quadrantSegments determines the maximum error in the approximation to the true buffer curve.

The default value of 8 gives less than 2% max error in the buffer distance.

For a max error of < 1%, use QS = 12. For a max error of < 0.1%, use QS = 18. The error is always less than the buffer distance (in other words, the computed buffer curve is always inside the true curve).
-}
  , quadrantSegments :: Int
{- | 
Sets whether the computed buffer should be single-sided. A single-sided buffer is constructed on only one side of each input line.

The side used is determined by the sign of the buffer distance:

* a positive distance indicates the left-hand side
* a negative distance indicates the right-hand side
The single-sided buffer of point geometries is the same as the regular buffer.

The End Cap Style for single-sided buffers is always ignored, and forced to the equivalent of 'FlatCap'.
-}
  , singleSided :: Bool
}

defaultBufferParams :: BufferParams
defaultBufferParams = BufferParams { joinStyle        = RoundJoin
                                   , capStyle         = RoundCap
                                   , mitreLimit       = 2.0
                                   , quadrantSegments = 8
                                   , singleSided      = False
                                   }

-- | Returns a Geometry that represents all points whose distance from this geometry is less than or equal to the given width.
buffer :: Geometry a -> Double -> BufferParams -> Some Geometry
buffer g width bp = runGeos $ do
  rg :: RG.Geom <- convertGeometryToRaw g
  rbp           <- R.createBufferParams
  R.setEndCapStyle rbp (convertCapStyle $ capStyle bp)
  R.setJoinStyle rbp (convertJoinStyle $ joinStyle bp)
  R.setMitreLimit rbp (mitreLimit bp)
  R.setQuadrantSegments rbp (quadrantSegments bp)
  R.setSingleSided rbp (singleSided bp)
  rg' <- R.buffer rg rbp width
  convertGeometryFromRaw rg'
