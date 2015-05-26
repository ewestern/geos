module GEOS.Raw.Base where
import qualified GEOS.Raw.Internal as I
import Foreign
import Foreign.C.String
import Data.Monoid ((<>))
import System.IO.Unsafe

newtype GEOSHandle = GEOSHandle { 
  _unGEOSHandle :: ForeignPtr I.GEOSContextHandle_HS 
}


makeMessageHandler :: (String -> IO ()) -> IO (FunPtr I.GEOSMessageHandler)
makeMessageHandler f =  I.mkMessageHandler (\cs -> peekCString cs >>= f) 
 
initializeGEOS :: (String -> IO ()) -> (String -> IO ()) -> GEOSHandle
initializeGEOS n e =  unsafePerformIO $ do
  -- must freePointer when done
  nh <- makeMessageHandler n
  eh <- makeMessageHandler e
  ptrC <- I.geos_initGEOS nh eh    
  fptr <- newForeignPtr I.geos_finishGEOS ptrC
  return $ GEOSHandle fptr

withHandle :: GEOSHandle -> (I.GEOSContextHandle_t -> IO a) -> IO a
withHandle (GEOSHandle ptr) f = withForeignPtr ptr f


throwIfZero :: (Eq a, Num a) => (a -> String) -> IO a -> IO a
throwIfZero f m = throwIf (\v -> v == 0) f m

mkErrorMessage :: Show a => String -> (a -> String) 
mkErrorMessage s = \n -> s  <> " has thrown an error:  " <> show n

