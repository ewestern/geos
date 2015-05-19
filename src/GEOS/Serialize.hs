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
newtype Writer = Writer { _unWriter :: ForeignPtr I.GEOSWKBWriter }

withReader (Reader r) f = withForeignPtr r f
withWriter (Writer w) f = withForeignPtr w f

createReader :: GEOSHandle -> IO Reader
createReader h = do 
    ptr <- throwIfNull "Create Reader" $ withHandle h $ \hp -> I.geos_WKBReaderCreate  hp  
    fp <- withHandle h $ \hp -> newForeignPtrEnv I.geos_WKBReaderDestroy hp ptr
    return $ Reader fp

read_ :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSWKBReader -> CString  -> CSize -> IO (Ptr I.GEOSGeometry)) 
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

createWriter :: GEOSHandle -> IO Writer
createWriter h = do
  ptr <- throwIfNull "CreateWriter" $ withHandle h $ \hp -> I.geos_WKBWriterCreate hp 
  fp <- withHandle h $ \hp -> newForeignPtrEnv I.geos_WKBWriterDestroy hp ptr
  return $ Writer fp
        
write_ :: (Ptr I.GEOSContextHandle -> Ptr I.GEOSWKBWriter -> Ptr I.GEOSGeometry -> Ptr CSize -> IO CString)
          -> GEOSHandle
          -> Writer
          -> Geometry
          -> IO BC.ByteString
write_ f h w g = do
  s <- withHandle h $ \hp ->
          withWriter w $ \wp ->
            withGeometry g $ \gp -> 
              alloca $ \lp ->
                f hp wp gp lp 
  bs <- BC.packCString s          
  return bs
                     
write :: GEOSHandle -> Writer -> Geometry -> IO BC.ByteString
write = write_ I.geos_WKBWriterWrite


writeHex :: GEOSHandle -> Writer -> Geometry -> IO BC.ByteString
writeHex = write_ I.geos_WKBWriterWriteHex
--
--- writers: must free GEOSFree
