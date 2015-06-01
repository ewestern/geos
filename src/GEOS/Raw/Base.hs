module GEOS.Raw.Base where
import qualified GEOS.Raw.Internal as I
import Foreign
import Foreign.C.String
import Data.Monoid ((<>))
import System.IO.Unsafe
import qualified Control.Concurrent.MVar as MV

newtype GEOSHandle = GEOSHandle { 
  _unGEOSHandle :: MV.MVar (ForeignPtr I.GEOSContextHandle_HS)
}

newtype Geos a = MyMonad { unMyMonad :: ReaderT GEOSHandle IO a }
  deriving (MonadReader GEOSHandle, Monad, Functor)

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
  mv <- newMvar fptr
  return $ GEOSHandle mv 

withHandle :: GEOSHandle -> (I.GEOSContextHandle_t -> IO a) -> IO a
withHandle (GEOSHandle mv) f = MV.withMVar mv $ \ptr -> withForeignPtr ptr f


throwIfZero :: (Eq a, Num a) => (a -> String) -> IO a -> IO a
throwIfZero f m = throwIf (\v -> v == 0) f m

mkErrorMessage :: Show a => String -> (a -> String) 
mkErrorMessage s = \n -> s  <> " has thrown an error:  " <> show n

