module GEOS.Wrapper where
import qualified GEOS.Internal as I
import qualified Data.Text as T
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe
import qualified Control.Concurrent.MVar as MV


newtype GEOSHandle = GEOSHandle { 
  _unGEOSHandle :: ForeignPtr I.GEOSContextHandle_HS 
}

newtype CoordinateSequence = CoordinateSequence { 
  _unCoordinateSequence :: ForeignPtr I.GEOSCoordSequence
}

newtype Geometry = Geometry { _unGeometry :: (ForeignPtr I.GEOSGeometry)}

makeMessageHandler :: (String -> IO ()) -> IO (FunPtr I.GEOSMessageHandler)
makeMessageHandler f =  I.mkMessageHandler (\cs -> peekCString cs >>= f) 
     
initializeGEOS :: (String -> IO ()) -> (String -> IO ()) -> IO GEOSHandle
initializeGEOS n e =  do
  -- must freePointer when done
  nh <- makeMessageHandler n
  eh <- makeMessageHandler e
  ptrC <- I.geos_initGEOS nh eh    
  fptr <- newForeignPtr I.geos_finishGEOS ptrC
  return $ GEOSHandle fptr

withHandle :: GEOSHandle -> (I.GEOSContextHandle_t -> IO a) -> IO a
withHandle (GEOSHandle ptr) f = withForeignPtr ptr f

withCoordinateSequence :: CoordinateSequence -> (Ptr I.GEOSCoordSequence -> IO a) -> IO a
withCoordinateSequence (CoordinateSequence fp) f = withForeignPtr fp f

withGeometry :: Geometry -> (Ptr I.GEOSGeometry -> IO a ) -> IO a
withGeometry (Geometry g) f = withForeignPtr g f

throwIfZero :: (Eq a, Num a) => (a -> String) -> IO a -> IO a
throwIfZero f m = throwIf (\v -> v == 0) f m
--- Info

getSRID :: GEOSHandle -> Geometry -> IO Int
getSRID h g = do
  s <- throwIfZero (\_ -> "Get SRID") $
        withHandle h $ \hp -> 
          withGeometry g $ \gp ->
            I.geos_GetSRID hp gp
  return $ fromIntegral s
    
setSRID :: GEOSHandle -> Geometry -> Int -> IO ()
setSRID h g i = withHandle h $ \hp -> 
                  withGeometry g $ \gp ->
                    I.geos_SetSRID hp gp $ fromIntegral i
  

getType :: GEOSHandle -> Geometry -> IO Int
getType h g = do
  i <- throwIfZero (\_ -> "Get Type") $
        withHandle h $ \hp -> 
          withGeometry g $ \gp ->
            I.geos_GeomType hp gp
  return $ fromIntegral i

getTypeId :: GEOSHandle -> Geometry -> IO Int
getTypeId h g = do
  i <- throwIfNeg (\_ -> "getTypeId") $
    withHandle h $ \hp ->
      withGeometry g $ \gp ->
        I.geos_GeomTypeId hp gp
  return $ fromIntegral i

-- this should be combined with clone to make it more flexible
getCoordinateSequence :: GEOSHandle -> Geometry -> IO CoordinateSequence
getCoordinateSequence h g = do
  ptr <- throwIfNull "Get CoordinateSequence" $ withHandle h $ \hp ->
          withGeometry g $ \gp ->
            I.geos_GetCoordSeq hp gp
  cloned <- throwIfNull "Clone Coordinate Sequence" $ withHandle h $ \hp ->
          I.geos_CoordSeqClone hp ptr
  fptr <- withHandle h $ \ch -> newForeignPtrEnv I.geos_CoordSeqDestroy ch cloned
  return $ CoordinateSequence fptr
      
getNumCoordinates :: GEOSHandle -> Geometry -> IO Int
getNumCoordinates h g = do 
  i <- throwIfNeg (\_ -> "getTypeId") $
    withHandle h $ \hp ->
      withGeometry g $ \gp ->
        I.geos_getNumCoordinates hp gp
  return $ fromIntegral i


--
-- Coordinate Sequence --

createCoordinateSequence :: GEOSHandle -> Int -> Int -> IO CoordinateSequence
createCoordinateSequence h size dim = do
    ptr <- throwIfNull "Create Coordinate Sequence" $ withHandle h $ \ptr -> I.geos_CoordSeqCreate ptr (fromIntegral size) (fromIntegral dim) 
    fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_CoordSeqDestroy ch ptr
    return $ CoordinateSequence  fp

setCoordinateSequence_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> CDouble -> IO CInt) -> GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequence_ f h cs idx val = do
  i <- throwIfZero (\_ -> "Cannot set coordinate sequence") $ 
        withHandle h (\ch -> withCoordinateSequence cs (\ pcs -> f ch pcs (fromIntegral idx) (realToFrac val)))
  return $ fromIntegral i

getCoordinateSequence_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt) 
                          -> GEOSHandle 
                          -> CoordinateSequence
                          -> Int
                          -> IO Double 
getCoordinateSequence_ f h cs idx = alloca $ \dptr -> do
  i <- throwIfZero (\_ -> "Cannot get coordinate value") $
      withHandle h $ \ch -> 
        withCoordinateSequence cs $ \pcs -> f ch pcs (fromIntegral idx) dptr
  d <- peek dptr
  return $ realToFrac d
  
getCoordinateSequenceX :: GEOSHandle -> CoordinateSequence -> Int -> IO Double
getCoordinateSequenceX = getCoordinateSequence_ I.geos_CoordSeqGetX

getCoordinateSequenceY :: GEOSHandle -> CoordinateSequence -> Int -> IO Double
getCoordinateSequenceY = getCoordinateSequence_ I.geos_CoordSeqGetY

getCoordinateSequenceZ :: GEOSHandle -> CoordinateSequence -> Int -> IO Double
getCoordinateSequenceZ = getCoordinateSequence_ I.geos_CoordSeqGetZ

getCoordinateSequenceSize :: GEOSHandle -> CoordinateSequence -> IO Int 
getCoordinateSequenceSize h c = alloca $ \ptr -> do
  i <- throwIfZero (\_ -> "Get Coordinate Sequence Size") $ 
        withHandle h $ \ch ->
          withCoordinateSequence c $ \pc ->
            I.geos_CoordSeqGetSize ch pc ptr
  s <- peek ptr
  return $ fromIntegral s

getCoordinateSequenceDimensions :: GEOSHandle -> CoordinateSequence -> IO Int 
getCoordinateSequenceDimensions h c = alloca $ \ptr -> do
  i <- throwIfZero (\_ -> "Get Coordinate Sequence Dimensions") $ 
        withHandle h $ \ch ->
          withCoordinateSequence c $ \pc ->
            I.geos_CoordSeqGetDimensions ch pc ptr
  s <- peek ptr
  return $ fromIntegral s

---

setCoordinateSequenceX :: GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequenceX = setCoordinateSequence_ I.geos_CoordSeqSetX

setCoordinateSequenceY :: GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequenceY = setCoordinateSequence_ I.geos_CoordSeqSetY

setCoordinateSequenceZ :: GEOSHandle -> CoordinateSequence -> Int -> Double -> IO Int  
setCoordinateSequenceZ = setCoordinateSequence_ I.geos_CoordSeqSetZ

------
--

createGeometry_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> IO (Ptr I.GEOSGeometry)) -> GEOSHandle -> CoordinateSequence -> IO Geometry
createGeometry_ f h c  = do
   g <- throwIfNull "Create Geometry" $ withCoordinateSequence c $ \pcs -> withHandle h $ \ch -> f ch pcs
   fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
   return $ Geometry fp

-- Geometry Constructors
createPoint :: GEOSHandle -> CoordinateSequence -> IO Geometry
createPoint = createGeometry_ I.geos_GeomCreatePoint

createLineString :: GEOSHandle -> CoordinateSequence -> IO Geometry
createLineString = createGeometry_ I.geos_GeomCreateLineString


