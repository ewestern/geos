module GEOS.Buffer (
    buffer
  , defaultBufferParams
  , bufferWithParams
  , BufferParams (..)
  , BufferJoinStyle (..)
  , BufferCapStyle (..)
) where
import qualified GEOS.Raw.Buffer as R
import GEOS.Raw.Base
import GEOS.Types
import GEOS.Geometry

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

buffer :: Geometry -> Double -> Int -> Geometry 
buffer g width quadsegs = runGeos $  do
    rg <- convertGeometryToRaw g
    rg' <- R.buffer rg width quadsegs
    convertGeometryFromRaw rg'

bufferWithParams :: Geometry -> Double -> BufferParams -> Geometry
bufferWithParams g width bp = runGeos $ do
  rg <- convertGeometryToRaw g
  rbp <- R.createBufferParams 
  R.setEndCapStyle rbp (convertCapStyle $ capStyle bp) 
  R.setJoinStyle rbp (convertJoinStyle $ joinStyle bp)
  R.setMitreLimit rbp (mitreLimit bp)
  R.setQuadrantSegments rbp (quadrantSegments bp)
  R.setSingleSided rbp (singleSided bp)
  rg' <- R.bufferWithParams rg rbp width  
  convertGeometryFromRaw rg'
