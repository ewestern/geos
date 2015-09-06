module GEOS.Raw.CoordSeq (
    CoordinateSequence (CoordinateSequence)
  , CoordSeqConst (CoordSeqConst)
  , withCoordinateSequence
  , withCoordSeqConst
  , createCoordinateSequence
  , getCoordinateSequenceX
  , getCoordinateSequenceY
  , getCoordinateSequenceZ
  , getCoordinateSequenceSize
  , getCoordinateSequenceDimensions
  , setCoordinateSequenceX
  , setCoordinateSequenceY
  , setCoordinateSequenceZ
  , setCoordinateSequenceOrd

) where
import qualified GEOS.Raw.Internal as I
import GEOS.Raw.Base
import Foreign
import Foreign.C.Types

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

createCoordinateSequence :: Int -> Int -> Geos CoordinateSequence
createCoordinateSequence size dim = do
  ptr <- withGeos $ \h ->
    throwIfNull "createCoordinateSequence" $ I.geos_CoordSeqCreate h (fromIntegral size) (fromIntegral dim)
  fp <- withGeos $ \h -> 
    newForeignPtrEnv I.geos_CoordSeqDestroy h ptr
  return $ CoordinateSequence fp



getCoordinateSequenceD_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt) 
                          -> CoordinateSequence
                          -> Int
                          -> Geos Double 
getCoordinateSequenceD_ f cs idx = withGeos $ \h -> 
  alloca $ \dptr -> do
    i <- throwIfZero (mkErrorMessage "getCoordiniateSequenceN") $
          withCoordinateSequence cs $ \pcs -> f h pcs (fromIntegral idx) dptr
    d <- peek dptr
    return $ realToFrac d
  
getCoordinateSequenceX :: CoordinateSequence -> Int -> Geos Double
getCoordinateSequenceX = getCoordinateSequenceD_ I.geos_CoordSeqGetX

getCoordinateSequenceY :: CoordinateSequence -> Int -> Geos Double
getCoordinateSequenceY = getCoordinateSequenceD_ I.geos_CoordSeqGetY

getCoordinateSequenceZ :: CoordinateSequence -> Int -> Geos Double
getCoordinateSequenceZ = getCoordinateSequenceD_ I.geos_CoordSeqGetZ

getCoordinateSequenceSize :: CoordinateSequence -> Geos Int 
getCoordinateSequenceSize c =  withGeos $ \h -> 
  alloca $ \ptr -> do
    i <- throwIfZero (mkErrorMessage "getCoordinateSequenceSize") $ 
          withCoordinateSequence c $ \pc ->
            I.geos_CoordSeqGetSize h pc ptr
    s <- peek ptr
    return $ fromIntegral s

getCoordinateSequenceDimensions :: CoordinateSequence -> Geos Int 
getCoordinateSequenceDimensions c = withGeos $ \h -> 
  alloca $ \ptr -> do
    i <- throwIfZero (mkErrorMessage "getCoordinateSeqenceDimensions") $ 
            withCoordinateSequence c $ \pc ->
              I.geos_CoordSeqGetDimensions h pc ptr
    s <- peek ptr
    return $ fromIntegral s

---
setCoordinateSequence_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> CDouble -> IO CInt) -> CoordinateSequence -> Int -> Double -> Geos ()
setCoordinateSequence_ f cs idx val = withGeos $ \h -> do
  i <- throwIfZero (mkErrorMessage "setCoordinateSEquenceN") $ 
        withCoordinateSequence cs $ \pcs -> 
          f h pcs (fromIntegral idx) (realToFrac val)
  return  ()


setCoordinateSequenceX :: CoordinateSequence -> Int -> Double -> Geos ()
setCoordinateSequenceX = setCoordinateSequence_ I.geos_CoordSeqSetX

setCoordinateSequenceY :: CoordinateSequence -> Int -> Double -> Geos ()
setCoordinateSequenceY = setCoordinateSequence_ I.geos_CoordSeqSetY

setCoordinateSequenceZ :: CoordinateSequence -> Int -> Double -> Geos () 
setCoordinateSequenceZ = setCoordinateSequence_ I.geos_CoordSeqSetZ

setCoordinateSequenceOrd :: CoordinateSequence -> Int -> Int  -> Double -> Geos Int
setCoordinateSequenceOrd cs idx dim v = withGeos $ \h -> do
  i <- throwIfZero (mkErrorMessage "setCoordinateSequenceN") $ 
          withCoordinateSequence cs $ \pcs -> 
            I.geos_CoordSeqSetOrdinate h pcs (fromIntegral idx) (fromIntegral dim) (realToFrac v)
  return $ fromIntegral i
