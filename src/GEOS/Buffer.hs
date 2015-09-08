module GEOS.Buffer (
    buffer
  , defaultBufferParams


) where
import qualified GEOS.Raw.Buffer as R
import qualified GEOS.Raw.Geometry as RG
import GEOS.Geometry
import GEOS.Raw.Base
import GEOS.Types

data BufferCapStyle = 
    RoundCap
  | SquareCap
  | FlatCap

data BufferJoinStyle =
    RoundJoin
  | MitreJoin
  | BevelJoin   

data BufferParams = BufferParams {
    joinStyle :: BufferJoinStyle
  , capStyle :: BufferCapStyle 
  , mitreLimit :: Double
  -- | The default number of facets into which to divide a fillet of 90 degrees.
  , quadrantSegments :: Int
  -- | True for single-sided, False otherwise
  , singleSided :: Bool
}

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

