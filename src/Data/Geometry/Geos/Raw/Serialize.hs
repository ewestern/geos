{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Geos.Raw.Serialize (
    createReader
  , createWriter
  , createWktReader
  , createWktWriter
  , read
  , readHex
  , readWkt
  , write
  , writeHex
  , writeWkt
) where
import Prelude hiding (read)
import qualified Data.Geometry.Geos.Raw.Internal as I
import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Raw.Geometry
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString.Char8 as BC
import Control.Exception (onException)


newtype Reader = Reader { _unReader :: ForeignPtr I.GEOSWKBReader }
newtype Writer = Writer { _unWriter :: ForeignPtr I.GEOSWKBWriter }

newtype WktReader = WktReader { _unWktReader :: ForeignPtr I.GEOSWKTReader }
newtype WktWriter = WktWriter { _unWktWriter :: ForeignPtr I.GEOSWKTWriter }

withWriter :: Writer -> (Ptr I.GEOSWKBWriter -> IO b) -> IO b
withWriter (Writer w) f = withForeignPtr w f

withWktReader :: WktReader -> (Ptr I.GEOSWKTReader -> IO b) -> IO b
withWktReader (WktReader r) f = withForeignPtr r f

withWktWriter :: WktWriter -> (Ptr I.GEOSWKTWriter -> IO b) -> IO b
withWktWriter (WktWriter w) f = withForeignPtr w f

createReader :: Geos Reader
createReader = withGeos $ \h -> do
    ptr <- throwIfNull "Create Reader" $ I.geos_WKBReaderCreate h
    Reader <$> newForeignPtrEnv I.geos_WKBReaderDestroy h ptr

createWktReader :: Geos WktReader
createWktReader = withGeos $ \h -> do
    ptr <- throwIfNull "Create WKT Reader" $ I.geos_WKTReaderCreate h
    WktReader <$> newForeignPtrEnv I.geos_WKTReaderDestroy h ptr

read_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSWKBReader -> CString  -> CSize -> IO (Ptr I.GEOSGeometry))
            -> Reader
            -> BC.ByteString
            -> Geos (Maybe Geom)
read_ f (Reader r) bs = withGeos $ \h ->
    onException (readBlock h) (pure Nothing)
      where
        readBlock h =  do
             ptr <- withForeignPtr r $ \rp ->
               BC.useAsCStringLen bs $
                 \(cs, l) -> f h rp cs $ fromIntegral l
             g <- wrapUpGeom h ptr
             pure g

wrapUpGeom :: I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO (Maybe Geom)
wrapUpGeom h ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just . Geom <$> newForeignPtrEnv I.geos_GeomDestroy h ptr

read :: Reader -> BC.ByteString -> Geos (Maybe Geom)
read = readWkb

readWkb :: Reader -> BC.ByteString -> Geos (Maybe Geom)
readWkb = read_ I.geos_WKBReaderRead

readHex :: Reader -> BC.ByteString -> Geos (Maybe Geom)
readHex  = read_ I.geos_WKBReaderReadHex

readWkt :: WktReader -> BC.ByteString -> Geos (Maybe Geom)
readWkt r bs = do
  withGeos $ \h -> do
    ptr <- withWktReader r $ \rp ->
              BC.useAsCString bs $ \cs ->
                I.geos_WKTReaderRead h rp cs
    g <- wrapUpGeom h ptr
    pure g

createWriter :: Geos Writer
createWriter = withGeos $ \h -> do
  ptr <- throwIfNull "CreateWriter" $ I.geos_WKBWriterCreate h
  I.geos_WKBWriterSetIncludeSRID h ptr $ fromBool True
  fp <- newForeignPtrEnv I.geos_WKBWriterDestroy h ptr
  return $ Writer fp

createWktWriter :: Geos WktWriter
createWktWriter = withGeos $ \h -> do
  ptr <- throwIfNull "CreateWktWriter" $ I.geos_WKTWriterCreate h
  fp <- newForeignPtrEnv I.geos_WKTWriterDestroy h ptr
  return $ WktWriter fp

write_ :: Geometry a
        => (I.GEOSContextHandle_t -> Ptr I.GEOSWKBWriter -> Ptr I.GEOSGeometry -> Ptr CSize -> IO CString)
        -> Writer
        -> a
        -> Geos BC.ByteString
write_ f w g = withGeos $ \h ->  do
  clen <- withWriter w $ \wp ->
          withGeometry g $ \gp ->
            alloca $ \lp -> do
              cs <- f h wp gp lp
              vl <- fromIntegral `fmap` (peek lp)
              return (cs, vl)
  bs <- BC.packCStringLen clen
  return bs

write :: Geometry a => Writer -> a -> Geos BC.ByteString
write = write_ I.geos_WKBWriterWrite

writeHex :: Geometry a => Writer -> a -> Geos BC.ByteString
writeHex = write_ I.geos_WKBWriterWriteHex

writeWkt :: Geometry a => WktWriter -> a -> Geos BC.ByteString
writeWkt w g = withGeos $ \h ->  do
  wkt <- withWktWriter w $ \wp ->
          withGeometry g $ \gp -> do
            cs <- I.geos_WKTWriterWrite h wp gp
            return cs
  bs <- BC.packCString wkt
  return bs
