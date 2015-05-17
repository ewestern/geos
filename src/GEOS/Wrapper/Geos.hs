module Geos.Wrapper.Geos where
import qualified Geos.Internal.Geos as I
import qualified Data.Text as T
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MV


newtype GEOSHandle = GEOSHandle { _unGEOSHandle :: MVar (ForeignPtr I.GEOSContextHandle) }
newtype CoordinateSequence = CoordinateSequence { _unCoordinateSequence :: (ForeignPtr I.GEOSCoordSequence)}
newtype Geometry = Geometry { _unGeometry :: (ForeignPtr I.GEOSGeometry)}

makeMessageHandler :: (String -> IO ()) -> IO (FunPtr I.GEOSMessageHandler)
makeMessageHandler f =  I.mkMessageHandler (\cs -> peekCString cs >>= f) 
     
initializeGEOS :: (String -> IO ()) -> (String -> IO ()) -> IO GEOSHandle
initializeGEOS n e =  do
  -- must freePointer when done
  nh <- makeMessageHandler n
  eh <- makeMessageHandler e
  ptrC <- I.geos_initGEOS_r nh eh    
  fptr <- newForeignPtr I.geos_finishGEOS_r ptrC
  mv <- MV.newMVar fptr
  return $ GEOSHandle mv

withHandle :: GEOSHandle -> (Ptr I.GEOSContextHandle -> IO a) -> IO a
withHandle (GEOSHandle mv) f = MV.withMVar mv $ \ptr -> withForeignPtr ptr f

withCoordinateSequence :: CoordinateSequence -> (Ptr I.GEOSCoordSequence -> IO a) -> IO a
withCoordinateSequence (CoordinateSequence cs) f = withForeignPtr cs f

-- Coordinate Sequence --

createCoordinateSequence :: GEOSHandle -> Int -> Int -> IO CoordinateSequence
createCoordinateSequence h size dim = do
    ptr <- throwIfNull "Create Coordinate Sequence" $ withHandle h $ \ptr -> I.geos_CoordSeqCreate ptr (fromIntegral size) (fromIntegral dim) 
    fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_CoordSeqDestroy ch ptr
    return $ CoordinateSequence fp

setCoordinateSequence_ :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSCoordSequence -> CUInt -> CDouble -> IO CInt) -> GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequence_ f h cs idx val = do
  i <- throwIf (\v -> v == 0) (\_ -> "Cannot set coordinate sequence") $ 
        withHandle h (\ch -> withCoordinateSequence cs (\ pcs -> f ch pcs (fromIntegral idx) (realToFrac val)))
  return $ fromIntegral i

getCoordinateSequence_ :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt) 
                          -> GEOSHandle 
                          -> CoordinateSequence
                          -> Int
                          -> IO Double 
getCoordinateSequence_ f h cs idx = alloca $ \dptr -> do
  i <- throwIf (\v -> v == 0) (\_ -> "Cannot get coordinate value") $
      withHandle h (\ch -> withCoordinateSequence cs (\pcs -> f ch pcs (fromIntegral idx) dptr))
  d <- peek dptr
  return $ realToFrac d
  
getCoordinateSequenceX :: GEOSHandle -> CoordinateSequence -> Int -> IO Double
getCoordinateSequenceX = getCoordinateSequence_ I.geos_CoordSeqGetX

getCoordinateSequenceY :: GEOSHandle -> CoordinateSequence -> Int -> IO Double
getCoordinateSequenceY = getCoordinateSequence_ I.geos_CoordSeqGetY

getCoordinateSequenceZ :: GEOSHandle -> CoordinateSequence -> Int -> IO Double
getCoordinateSequenceZ = getCoordinateSequence_ I.geos_CoordSeqGetZ
---

setCoordinateSequenceX :: GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequenceX = setCoordinateSequence_ I.geos_CoordSeqSetX

setCoordinateSequenceY :: GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequenceY = setCoordinateSequence_ I.geos_CoordSeqSetY

setCoordinateSequenceZ :: GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequenceZ = setCoordinateSequence_ I.geos_CoordSeqSetZ

createGeometry_ :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSCoordSequence -> IO (Ptr I.GEOSGeometry)) -> GEOSHandle -> CoordinateSequence -> IO Geometry
createGeometry_ f h (CoordinateSequence cs) = do
   g <- throwIfNull "Create Geometry" $ withForeignPtr cs $ \pcs -> withHandle h $ \ch -> f ch pcs
   fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
   return $ Geometry fp

-- Geometry Constructors
createPoint :: GEOSHandle -> CoordinateSequence -> IO Geometry
createPoint = createGeometry_ I.geos_GeomCreatePoint

createLineString :: GEOSHandle -> CoordinateSequence -> IO Geometry
createLineString = createGeometry_ I.geos_GeomCreateLineString


