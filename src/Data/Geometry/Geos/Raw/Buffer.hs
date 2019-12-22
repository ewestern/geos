module Data.Geometry.Geos.Raw.Buffer
  ( BufferParams
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
  )
where
import           Data.Geometry.Geos.Raw.Internal
import qualified Data.Geometry.Geos.Raw.Geometry
                                               as RG
import           Data.Geometry.Geos.Raw.Base
import           Foreign                 hiding ( throwIfNull
                                                , void
                                                )
import           Control.Monad                  ( void )

newtype BufferParams =  BufferParams (ForeignPtr GEOSBufferParams) deriving (Show, Eq)


withBufferParams :: BufferParams -> (Ptr GEOSBufferParams -> IO a) -> IO a
withBufferParams (BufferParams g) = withForeignPtr g


createBufferParams :: Geos BufferParams
createBufferParams = withGeos $ \h -> do
  bp <- geos_BufferParamsCreate h
  fp <- newForeignPtrEnv geos_BufferParamsDestroy h bp
  return $ BufferParams fp


setEndCapStyle :: BufferParams -> BufferCapStyle -> Geos ()
setEndCapStyle b s =
  void $ throwIfZero (mkErrorMessage "setEndCapStyle") $ withGeos $ \h ->
    withBufferParams b
      $ \bp -> geos_BufferParamsSetEndCapStyle h bp $ unBufferCapStyle s


setJoinStyle :: BufferParams -> BufferJoinStyle -> Geos ()
setJoinStyle b s =
  void $ throwIfZero (mkErrorMessage "setJoinStyle") $ withGeos $ \h ->
    withBufferParams b
      $ \bp -> geos_BufferParamsSetJoinStyle h bp $ unBufferJoinStyle s


setMitreLimit :: BufferParams -> Double -> Geos ()
setMitreLimit b d =
  void $ throwIfZero (mkErrorMessage "setJoinStyle") $ withGeos $ \h ->
    withBufferParams b
      $ \bp -> geos_BufferParamsSetMitreLimit h bp $ realToFrac d

setQuadrantSegments :: BufferParams -> Int -> Geos ()
setQuadrantSegments b i =
  void $ throwIfZero (mkErrorMessage "setJoinStyle") $ withGeos $ \h ->
    withBufferParams b
      $ \bp -> geos_BufferParamsSetQuadrantSegments h bp $ fromIntegral i

setSingleSided :: BufferParams -> Bool -> Geos ()
setSingleSided bp b =
  void $ throwIfZero (mkErrorMessage "setSingleSided") $ withGeos $ \h ->
    withBufferParams bp
      $ \bpp -> geos_BufferParamsSetSingleSided h bpp $ fromBool b

buffer :: RG.Geometry a => a -> BufferParams -> Double -> Geos a
buffer g b width = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "bufferWithParams" $ RG.withGeometry g $ \gp ->
    withBufferParams b $ flip (geos_BufferWithParams h gp) $ realToFrac width
  traverse (RG.constructGeometry h) eitherPtr

bufferWithStyle
  :: RG.Geometry a
  => a
  -> Double
  -> Int
  -> BufferCapStyle
  -> BufferJoinStyle
  -> Double
  -> Geos a
bufferWithStyle g width quadsegs capstyle joinstyle mitrelimit =
  withGeos' $ \h -> do
    eitherPtr <- throwIfNull' "bufferWithStyle" $ RG.withGeometry g $ \gp ->
      geos_BufferWithStyle h
                           gp
                           (realToFrac width)
                           (fromIntegral quadsegs)
                           (unBufferCapStyle capstyle)
                           (unBufferJoinStyle joinstyle)
        $ realToFrac mitrelimit
    traverse (RG.constructGeometry h) eitherPtr

-- | Will only accept LineString geometries. For the 'width' parameter, negative doubles represent a right-side offset, and positive doubles represent a left-side offset. 
offsetCurve
  :: RG.Geometry a => a -> Double -> Int -> BufferJoinStyle -> Double -> Geos a
offsetCurve g width quadsegs joinstyle mitrelimit = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "offsetCurve" $ RG.withGeometry g $ \gp ->
    geos_OffsetCurve h
                     gp
                     (realToFrac width)
                     (fromIntegral quadsegs)
                     (unBufferJoinStyle joinstyle)
      $ realToFrac mitrelimit
  traverse (RG.constructGeometry h) eitherPtr
