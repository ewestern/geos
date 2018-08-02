module Data.Geometry.Geos.Raw.Buffer (
    BufferParams
  , withBufferParams
  , BufferCapStyle
  , BufferJoinStyle
  , buffer
  , createBufferParams
  , setEndCapStyle
  , setJoinStyle
  , setMitreLimit
  , setQuadrantSegments
  , setSingleSided
  , bufferWithStyle
  , offsetCurve
  , capRound
  , capFlat
  , capSquare
  , joinRound
  , joinMitre
  , joinBevel
)  where
import Data.Geometry.Geos.Raw.Internal 
import qualified Data.Geometry.Geos.Raw.Geometry as RG
import Data.Geometry.Geos.Raw.Base
import Foreign

newtype BufferParams =  BufferParams (ForeignPtr GEOSBufferParams) deriving (Show, Eq)


withBufferParams :: BufferParams -> (Ptr GEOSBufferParams -> IO a ) -> IO a
withBufferParams (BufferParams g) f = withForeignPtr g f


createBufferParams :: Geos BufferParams 
createBufferParams = withGeos $ \h -> do
  bp <- geos_BufferParamsCreate h
  fp <- newForeignPtrEnv geos_BufferParamsDestroy h bp
  return $ BufferParams fp


setEndCapStyle :: BufferParams -> BufferCapStyle -> Geos ()
setEndCapStyle b s = withGeos $ \h -> do
  _ <- throwIfZero (mkErrorMessage "setEndCapStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetEndCapStyle h bp $ unBufferCapStyle s  
  return ()

setJoinStyle :: BufferParams -> BufferJoinStyle -> Geos ()
setJoinStyle b s = withGeos $ \h -> do
  _ <- throwIfZero (mkErrorMessage "setJoinStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetJoinStyle h bp $ unBufferJoinStyle s  
  return ()


setMitreLimit :: BufferParams -> Double -> Geos ()
setMitreLimit b d = withGeos $ \h -> do
  _ <- throwIfZero (mkErrorMessage "setJoinStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetMitreLimit h bp $ realToFrac d  
  return ()

setQuadrantSegments :: BufferParams -> Int -> Geos ()
setQuadrantSegments b i = withGeos $ \h -> do
  _ <- throwIfZero (mkErrorMessage "setJoinStyle") $ withBufferParams b $ \bp -> 
      geos_BufferParamsSetQuadrantSegments h bp $ fromIntegral i 
  return ()

setSingleSided :: BufferParams -> Bool -> Geos ()
setSingleSided bp b = withGeos $ \h -> do
  _ <- throwIfZero (mkErrorMessage "setSingleSided") $ withBufferParams bp $ \bpp ->
    geos_BufferParamsSetSingleSided h bpp $ fromBool b
  return ()

buffer :: RG.Geometry a => a -> BufferParams -> Double -> Geos a
buffer g b width = 
  withGeos $ \h -> do
    RG.withGeometry g $ \gp ->
      withBufferParams b $ \bp ->  do
        g' <- throwIfNull "bufferWithParams" $ geos_BufferWithParams h gp bp $ realToFrac width
        RG.constructGeometry h g'

bufferWithStyle :: RG.Geometry a => a -> Double -> Int -> BufferCapStyle -> BufferJoinStyle -> Double -> Geos a
bufferWithStyle g width quadsegs capstyle joinstyle mitrelimit = 
  withGeos $ \h -> 
    RG.withGeometry g $ \gp -> do
      ptr <- throwIfNull "bufferWithStyle" $ geos_BufferWithStyle h gp (realToFrac width) (fromIntegral quadsegs) (unBufferCapStyle capstyle) (unBufferJoinStyle joinstyle) $ realToFrac mitrelimit
      RG.constructGeometry h ptr

-- | Will only accept LineString geometries. For the 'width' parameter, negative doubles represent a right-side offset, and positive doubles represent a left-side offset. 
offsetCurve :: RG.Geometry a => a -> Double -> Int -> BufferJoinStyle -> Double -> Geos a
offsetCurve g width quadsegs joinstyle mitrelimit = 
  withGeos $ \h -> 
    RG.withGeometry g $ \gp -> do
      ptr <- throwIfNull "offsetCurve" $ geos_OffsetCurve h gp (realToFrac width) (fromIntegral quadsegs) (unBufferJoinStyle joinstyle) $ realToFrac mitrelimit
      RG.constructGeometry h ptr
