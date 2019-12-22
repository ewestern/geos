{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Geos.Raw.Serialize
  ( createReader
  , createWriter
  , createWktReader
  , createWktWriter
  , read
  , readHex
  , readWkt
  , write
  , writeHex
  , writeWkt
  )
where
import           Prelude                 hiding ( read )
import qualified Data.Geometry.Geos.Raw.Internal
                                               as I
import           Data.Geometry.Geos.Raw.Base
import           Data.Geometry.Geos.Raw.Geometry
import           Foreign                 hiding ( throwIfNull )
import           Foreign.C.String
import           Foreign.C.Types
import qualified Data.ByteString.Char8         as BC


newtype Reader = Reader { _unReader :: ForeignPtr I.GEOSWKBReader }
newtype Writer = Writer { _unWriter :: ForeignPtr I.GEOSWKBWriter }

newtype WktReader = WktReader { _unWktReader :: ForeignPtr I.GEOSWKTReader }
newtype WktWriter = WktWriter { _unWktWriter :: ForeignPtr I.GEOSWKTWriter }

createReader :: Geos Reader
createReader = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "Create Reader" $ I.geos_WKBReaderCreate h
  traverse (fmap Reader . newForeignPtrEnv I.geos_WKBReaderDestroy h) eitherPtr

createWktReader :: Geos WktReader
createWktReader = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "Create WKT Reader" $ I.geos_WKTReaderCreate h
  traverse (fmap WktReader . newForeignPtrEnv I.geos_WKTReaderDestroy h)
           eitherPtr

read_
  :: (  I.GEOSContextHandle_t
     -> Ptr I.GEOSWKBReader
     -> CString
     -> CSize
     -> IO (Ptr I.GEOSGeometry)
     )
  -> Reader
  -> BC.ByteString
  -> Geos Geom
read_ f (Reader r) bs = withGeos' $ \h -> do
  eitherPtr <- readBlock h
  traverse (constructGeometry h) eitherPtr
 where
  readBlock h = throwIfNull' "read_" $ withForeignPtr r $ \rp ->
    BC.useAsCStringLen bs $ \(cs, l) -> f h rp cs $ fromIntegral l

read :: Reader -> BC.ByteString -> Geos Geom
read = readWkb

readWkb :: Reader -> BC.ByteString -> Geos Geom
readWkb = read_ I.geos_WKBReaderRead

readHex :: Reader -> BC.ByteString -> Geos Geom
readHex = read_ I.geos_WKBReaderReadHex

readWkt :: WktReader -> BC.ByteString -> Geos Geom
readWkt (WktReader r) bs = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "readWKT" $ withForeignPtr r $ \rp ->
    BC.useAsCString bs $ \cs -> I.geos_WKTReaderRead h rp cs
  traverse (constructGeometry h) eitherPtr

createWriter :: Geos Writer
createWriter = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "CreateWriter" $ I.geos_WKBWriterCreate h
  traverse (create h) eitherPtr
 where
  create h ptr = do
    I.geos_WKBWriterSetIncludeSRID h ptr $ fromBool True
    fp <- newForeignPtrEnv I.geos_WKBWriterDestroy h ptr
    pure $ Writer fp


createWktWriter :: Geos WktWriter
createWktWriter = withGeos' $ \h -> do
  eitherPtr <- throwIfNull' "CreateWktWriter" $ I.geos_WKTWriterCreate h
  traverse (fmap WktWriter . newForeignPtrEnv I.geos_WKTWriterDestroy h)
           eitherPtr

write_
  :: Geometry a
  => (  I.GEOSContextHandle_t
     -> Ptr I.GEOSWKBWriter
     -> Ptr I.GEOSGeometry
     -> Ptr CSize
     -> IO CString
     )
  -> Writer
  -> a
  -> Geos BC.ByteString
write_ f (Writer w) g = withGeos $ \h -> do
  clen <- withForeignPtr w $ \wp -> withGeometry g $ \gp -> alloca $ \lp -> do
    cs <- f h wp gp lp
    vl <- fromIntegral `fmap` peek lp
    pure (cs, vl)
  BC.packCStringLen clen

write :: Geometry a => Writer -> a -> Geos BC.ByteString
write = write_ I.geos_WKBWriterWrite

writeHex :: Geometry a => Writer -> a -> Geos BC.ByteString
writeHex = write_ I.geos_WKBWriterWriteHex

writeWkt :: Geometry a => WktWriter -> a -> Geos BC.ByteString
writeWkt (WktWriter w) g = withGeos $ \h -> do
  wkt <- withForeignPtr w
    $ \wp -> withGeometry g $ \gp -> I.geos_WKTWriterWrite h wp gp
  BC.packCString wkt
