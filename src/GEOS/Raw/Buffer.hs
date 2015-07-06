module GEOS.Raw.Buffer (
    BufferParams
  , withBufferParams
  , buffer
  , createBufferParams
  , setEndCapStyle
  , setJoinStyle
  , setMitreLimit
  , setQuadrantSegments
  , setSingleSided
  , bufferWithStyle
  , bufferWithParams
  , offsetCurve
  , capRound
  , capFlat
  , capSquare
  , joinRound
  , joinMitre
  , joinBevel
)  where
import GEOS.Raw.Internal 
import qualified GEOS.Raw.Geometry as RG
import GEOS.Geometry
import GEOS.Raw.Base
import GEOS.Raw.CoordSeq
import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Data.Monoid ((<>))
import qualified Data.Vector as V

newtype BufferParams = BufferParams {
  unBufferParams :: ForeignPtr GEOSBufferParams 
} deriving (Show, Eq)


withBufferParams :: BufferParams -> (Ptr GEOSBufferParams -> IO a ) -> IO a
withBufferParams (BufferParams g) f = withForeignPtr g f



-- | Create a buffer around a geoemtry, where quadsegs is the number of line segments to use to approximate a quarter of a circle.
buffer :: RG.Geometry -> Double -> Int -> Geos RG.Geometry
buffer geo width quadsegs = withGeos $ \h -> do 
  g <- RG.withGeometry geo $ \gp -> geos_Buffer h gp (realToFrac width) $ fromIntegral quadsegs
  fp <- newForeignPtrEnv geos_GeomDestroy h g
  return $ RG.Geometry fp


createBufferParams :: Geos BufferParams 
createBufferParams = withGeos $ \h -> do
  bp <- geos_BufferParamsCreate h
  fp <- newForeignPtrEnv geos_BufferParamsDestroy h bp
  return $ BufferParams fp


setEndCapStyle :: BufferParams -> BufferCapStyle -> Geos ()
setEndCapStyle b s = withGeos $ \h -> do
  throwIfZero (mkErrorMessage "setEndCapStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetEndCapStyle h bp $ unBufferCapStyle s  
  return ()

setJoinStyle :: BufferParams -> BufferJoinStyle -> Geos ()
setJoinStyle b s = withGeos $ \h -> do
  throwIfZero (mkErrorMessage "setJoinStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetJoinStyle h bp $ unBufferJoinStyle s  
  return ()


setMitreLimit :: BufferParams -> Double -> Geos ()
setMitreLimit b d = withGeos $ \h -> do
  throwIfZero (mkErrorMessage "setJoinStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetMitreLimit h bp $ realToFrac d  
  return ()

setQuadrantSegments :: BufferParams -> Int -> Geos ()
setQuadrantSegments b i = withGeos $ \h -> do
  throwIfZero (mkErrorMessage "setJoinStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetQuadrantSegments h bp $ fromIntegral i 
  return ()

setSingleSided :: BufferParams -> Bool -> Geos ()
setSingleSided bp b = withGeos $ \h -> do
  throwIfZero (mkErrorMessage "setSingleSided") $ withBufferParams bp $ \bpp ->
    geos_BufferParamsSetSingleSided h bpp $ fromBool b
  return ()

bufferWithParams :: RG.Geometry -> BufferParams -> Double -> Geos RG.Geometry
bufferWithParams g b width = withGeos $ \h -> do
  g <- throwIfNull "bufferWithParams" $ RG.withGeometry g $ \gp ->
    withBufferParams b $ \bp -> 
      geos_BufferWithParams h gp bp $ realToFrac width
  fp <- newForeignPtrEnv geos_GeomDestroy h g
  return $ RG.Geometry fp

bufferWithStyle :: RG.Geometry -> Double -> Int -> BufferCapStyle -> BufferJoinStyle -> Double -> Geos RG.Geometry
bufferWithStyle g width quadsegs capstyle joinstyle mitrelimit = withGeos $ \h -> do
  g <- throwIfNull "bufferWithStyle" $ RG.withGeometry g $ \gp ->
        geos_BufferWithStyle h gp (realToFrac width) (fromIntegral quadsegs) (unBufferCapStyle capstyle) (unBufferJoinStyle joinstyle) $ realToFrac mitrelimit
  fp <- newForeignPtrEnv geos_GeomDestroy h g
  return $ RG.Geometry fp

-- | Will only accept LineString geometries. For the 'width' parameter, negative doubles represent a right-side offset, and positive doubles represent a left-side offset. 
offsetCurve :: RG.Geometry -> Double -> Int -> BufferJoinStyle -> Double -> Geos RG.Geometry 
offsetCurve g width quadsegs joinstyle mitrelimit = withGeos $ \h -> do
  g <- throwIfNull "offsetCurve" $ RG.withGeometry g $ \gp ->
    geos_OffsetCurve h gp (realToFrac width) (fromIntegral quadsegs) (unBufferJoinStyle joinstyle) $ realToFrac mitrelimit
  fp <- newForeignPtrEnv geos_GeomDestroy h g
  return $ RG.Geometry fp


