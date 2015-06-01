{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module GEOS.Raw.Base where
import qualified GEOS.Raw.Internal as I
import Foreign
import Foreign.C.String
import Data.Monoid ((<>))
import System.IO.Unsafe
import qualified Control.Concurrent.MVar as MV
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Applicative (Applicative)

newtype GEOSHandle = GEOSHandle { 
  unGEOSHandle :: MV.MVar (ForeignPtr I.GEOSContextHandle_HS)
}

newtype Geos a = Geos { unGeos :: ReaderT GEOSHandle IO a }
  deriving (MonadReader GEOSHandle, Monad, Functor, Applicative)
-- don't derive MonadIO to restrict user from performing arbitrary IO

{-runGeos :: Geos a -> a-}
{-withGeos-}

makeMessageHandler :: (String -> IO ()) -> IO (FunPtr I.GEOSMessageHandler)
makeMessageHandler f =  I.mkMessageHandler (\cs -> peekCString cs >>= f) 
 
initializeGEOS :: (String -> IO ()) -> (String -> IO ()) -> IO GEOSHandle
initializeGEOS n e =   do
  -- must freePointer when done
  nh <- makeMessageHandler n
  eh <- makeMessageHandler e
  ptrC <- I.geos_initGEOS nh eh    
  fptr <- newForeignPtr I.geos_finishGEOS ptrC
  mv <- MV.newMVar fptr
  return $ GEOSHandle mv 

withHandle :: GEOSHandle -> (I.GEOSContextHandle_t -> IO a) -> IO a
withHandle (GEOSHandle mv) f =  MV.withMVar mv $ \ptr -> withForeignPtr ptr f

withGeos :: (I.GEOSContextHandle_t -> IO a) -> Geos a
withGeos f =  do
  mv <- asks unGEOSHandle
  Geos . ReaderT $ \gh -> MV.withMVar mv $ \fp -> withForeignPtr fp f

{-newGeos :: a -> Geos a-}
{-runGeos :: Geos a -> a-}


throwIfZero :: (Eq a, Num a) => (a -> String) -> IO a -> IO a
throwIfZero f m = throwIf (\v -> v == 0) f m

mkErrorMessage :: Show a => String -> (a -> String) 
mkErrorMessage s = \n -> s  <> " has thrown an error:  " <> show n


-- test:w
--
newtype CoordinateSequence = CoordinateSequence { 
  _unCoordinateSequence :: ForeignPtr I.GEOSCoordSequence
}

createCoordinateSequence :: Int -> Int -> Geos CoordinateSequence
createCoordinateSequence size dim = do
  ptr <- withGeos $ \h ->
    throwIfNull "createCoordinateSequence" $ I.geos_CoordSeqCreate h (fromIntegral size) (fromIntegral dim)
  fp <- withGeos $ \h -> 
    newForeignPtrEnv I.geos_CoordSeqDestroy h ptr
  return $ CoordinateSequence fp

    {-ptr <- throwIfNull "createCoordinateSequence" $ withHandle h $ \ptr -> I.geos_CoordSeqCreate ptr (fromIntegral size) (fromIntegral dim) -}



