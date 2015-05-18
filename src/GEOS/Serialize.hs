module GEOS.Serialize where
import qualified GEOS.Internal as I
import GEOS.Wrapper
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.ByteString.Char8 as BC

{-newtype Geometry = Geometry { _unGeometry :: (ForeignPtr I.GEOSGeometry)}-}

newtype Reader = Reader { _unReader :: ForeignPtr I.GEOSWKBReader }

withReader (Reader r) f = withForeignPtr r f

createReader :: GEOSHandle -> IO Reader
createReader h = do 
    ptr <- throwIfNull "Create Reader" $ withHandle h $ \hp -> I.geos_WKBReaderCreate  hp  
    fp <- withHandle h $ \hp -> newForeignPtrEnv I.geos_WKBReaderDestroy hp ptr
    return $ Reader fp



read_ :: (Ptr GEOSContextHandle -> Ptr GEOSWKBReader -> CString  -> CSize -> IO (Ptr GEOSGeometry)) 
            -> GEOSHandle 
            -> Reader 
            -> BC.ByteString 
            -> IO Geometry
read_ f h r bs = do
  ptr <- withHandle h $ \hp -> 
            withReader r $ \rp -> 
              BC.useAsCStringLen bs $ \(cs, l) -> 
                f hp rp cs $ fromIntegral l 
  fp <- withHandle h $ \hp -> newForeignPtrEnv I.geos_GeomDestroy hp ptr
  return $ Geometry fp
  

read :: GEOSHandle -> Reader -> BC.ByteString -> IO Geometry
read = read_ I.geos_WKBReaderRead
  
readHex :: GEOSHandle -> Reader -> BC.ByteString -> IO Geometry
readHex = read_ I.geos_WKBReaderReadHex

--- writers: must free GEOSFree
