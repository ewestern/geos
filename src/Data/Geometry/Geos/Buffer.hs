{-# LANGUAGE ScopedTypeVariables #-}

module Data.Geometry.Geos.Buffer (
    buffer
  , defaultBufferParams
  , bufferWithParams
  , BufferParams (..)
  , BufferJoinStyle (..)
  , BufferCapStyle (..)
) where
import qualified Data.Geometry.Geos.Raw.Buffer as R
import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Types
import Data.Geometry.Geos.Geometry
import qualified Data.Geometry.Geos.Raw.Geometry as RG

data BufferCapStyle = 
    RoundCap
  | SquareCap
  | FlatCap

data BufferJoinStyle =
    RoundJoin
  | MitreJoin
  | BevelJoin   

convertCapStyle :: BufferCapStyle -> R.BufferCapStyle
convertCapStyle = \case
  RoundCap -> R.capRound
  SquareCap -> R.capSquare
  FlatCap -> R.capFlat

convertJoinStyle :: BufferJoinStyle -> R.BufferJoinStyle
convertJoinStyle = \case
  RoundJoin -> R.joinRound
  MitreJoin -> R.joinMitre
  BevelJoin -> R.joinBevel

data BufferParams = BufferParams {
    joinStyle :: BufferJoinStyle
  , capStyle :: BufferCapStyle 
  , mitreLimit :: Double
  -- | The default number of facets into which to divide a fillet of 90 degrees.
  , quadrantSegments :: Int
  -- | True for single-sided, False otherwise
  , singleSided :: Bool
}

defaultBufferParams :: BufferParams
defaultBufferParams = BufferParams {
    joinStyle = RoundJoin
  , capStyle = RoundCap
  , mitreLimit = 2.0 
  , quadrantSegments = 8
  , singleSided = False
}

-- | Returns a Geometry that represents all points whose distance from this geometry is less than or equal to the given width. The quadsegs argument sets the number of segments used to approximate a quarter circle.

buffer :: Geometry a -> Double -> Int -> Some Geometry 
buffer g width quadsegs = runGeos $  do
    rg :: RG.Geom <- convertGeometryToRaw g
    rg' <- R.buffer rg width quadsegs
    convertGeometryFromRaw rg'

bufferWithParams :: Geometry a -> Double -> BufferParams -> Some Geometry
bufferWithParams g width bp = runGeos $ do
  rg :: RG.Geom <- convertGeometryToRaw g
  rbp <- R.createBufferParams 
  R.setEndCapStyle rbp (convertCapStyle $ capStyle bp) 
  R.setJoinStyle rbp (convertJoinStyle $ joinStyle bp)
  R.setMitreLimit rbp (mitreLimit bp)
  R.setQuadrantSegments rbp (quadrantSegments bp)
  R.setSingleSided rbp (singleSided bp)
  rg' <- R.bufferWithParams rg rbp width  
  convertGeometryFromRaw rg'
