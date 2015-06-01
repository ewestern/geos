module GEOS.Raw.CoordSeq where
import qualified GEOS.Raw.Internal as I
import GEOS.Raw.Base
import Foreign
import Foreign.ForeignPtr
import Foreign.C.Types
import System.IO.Unsafe

newtype CoordinateSequence = CoordinateSequence { 
  _unCoordinateSequence :: ForeignPtr I.GEOSCoordSequence
}
newtype CoordSeqConst = CoordSeqConst {
  _unCoordSeqConst :: Ptr I.GEOSCoordSequence
}

withCoordSeqConst :: CoordSeqConst -> (Ptr I.GEOSCoordSequence -> IO a) -> IO a
withCoordSeqConst (CoordSeqConst p) f = f p
    
withCoordinateSequence :: CoordinateSequence -> (Ptr I.GEOSCoordSequence -> IO a) -> IO a
withCoordinateSequence (CoordinateSequence fp) f = withForeignPtr fp f


createCoordinateSequence :: GEOSHandle -> Int -> Int -> CoordinateSequence
createCoordinateSequence h size dim = unsafePerformIO $ do
    ptr <- throwIfNull "createCoordinateSequence" $ withHandle h $ \ptr -> I.geos_CoordSeqCreate ptr (fromIntegral size) (fromIntegral dim) 
    fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_CoordSeqDestroy ch ptr
    return $ CoordinateSequence  fp

getCoordinateSequenceD_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt) 
                          -> GEOSHandle 
                          -> CoordinateSequence
                          -> Int
                          -> Double 
getCoordinateSequenceD_ f h cs idx = unsafePerformIO $ alloca $ \dptr -> do
  i <- throwIfZero (mkErrorMessage "getCoordiniateSequenceN") $
      withHandle h $ \ch -> 
        withCoordinateSequence cs $ \pcs -> f ch pcs (fromIntegral idx) dptr
  d <- peek dptr
  return $ realToFrac d
  
getCoordinateSequenceX :: GEOSHandle -> CoordinateSequence -> Int -> Double
getCoordinateSequenceX = getCoordinateSequenceD_ I.geos_CoordSeqGetX

getCoordinateSequenceY :: GEOSHandle -> CoordinateSequence -> Int -> Double
getCoordinateSequenceY = getCoordinateSequenceD_ I.geos_CoordSeqGetY

getCoordinateSequenceZ :: GEOSHandle -> CoordinateSequence -> Int -> Double
getCoordinateSequenceZ = getCoordinateSequenceD_ I.geos_CoordSeqGetZ

getCoordinateSequenceSize :: GEOSHandle -> CoordinateSequence -> Int 
getCoordinateSequenceSize h c = unsafePerformIO $ alloca $ \ptr -> do
  i <- throwIfZero (mkErrorMessage "getCoordinateSequenceSize") $ 
        withHandle h $ \ch ->
          withCoordinateSequence c $ \pc ->
            I.geos_CoordSeqGetSize ch pc ptr
  s <- peek ptr
  return $ fromIntegral s

getCoordinateSequenceDimensions :: GEOSHandle -> CoordinateSequence -> Int 
getCoordinateSequenceDimensions h c = unsafePerformIO $ alloca $ \ptr -> do
  i <- throwIfZero (mkErrorMessage "getCoordinateSeqenceDimensions") $ 
        withHandle h $ \ch ->
          withCoordinateSequence c $ \pc ->
            I.geos_CoordSeqGetDimensions ch pc ptr
  s <- peek ptr
  return $ fromIntegral s

---
setCoordinateSequence_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> CDouble -> IO CInt) -> GEOSHandle -> CoordinateSequence -> Int -> Double -> Int  
setCoordinateSequence_ f h cs idx val = unsafePerformIO $ do
  i <- throwIfZero (mkErrorMessage "setCoordinateSEquenceN") $ 
        withHandle h (\ch -> withCoordinateSequence cs (\ pcs -> f ch pcs (fromIntegral idx) (realToFrac val)))
  return  ()


setCoordinateSequenceX :: GEOSHandle -> CoordinateSequence -> Int -> Double -> ()
setCoordinateSequenceX = setCoordinateSequence_ I.geos_CoordSeqSetX

setCoordinateSequenceY :: GEOSHandle -> CoordinateSequence -> Int -> Double -> ()
setCoordinateSequenceY = setCoordinateSequence_ I.geos_CoordSeqSetY

setCoordinateSequenceZ :: GEOSHandle -> CoordinateSequence -> Int -> Double -> ()  
setCoordinateSequenceZ = setCoordinateSequence_ I.geos_CoordSeqSetZ

setCoordinateSequenceOrd :: GEOSHandle -> CoordinateSequence -> Int -> Int  -> Double -> Int
setCoordinateSequenceOrd h cs idx dim v = unsafePerformIO $ do
  i <- throwIfZero (mkErrorMessage "setCoordinateSequenceN") $ 
        withHandle h $ \ch -> 
          withCoordinateSequence cs $ \pcs -> 
            I.geos_CoordSeqSetOrdinate ch pcs (fromIntegral idx) (fromIntegral dim) (realToFrac v)
  return $ fromIntegral i
