{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-} 

module GEOS.Raw.Base (
    Geos
  , runGeos  
  , throwIfZero
  , withGeos
  , mkErrorMessage
  , (>><)
) where
import qualified GEOS.Raw.Internal as I
import Foreign
import Foreign.C.String
import Data.Monoid ((<>))
import System.IO.Unsafe
import qualified Control.Concurrent.MVar as MV
import Control.Monad.Reader
{-import Control.Monad.IO.Class-}
import Control.Applicative (Applicative)

infixl 1 >><
(>><) :: Monad m => m a -> (a -> m b) -> m a
m >>< f = (m >>= f) >> m



newtype GEOSHandle = GEOSHandle { 
  unGEOSHandle :: MV.MVar (ForeignPtr I.GEOSContextHandle_HS)
}

newtype Geos a = Geos { unGeos :: ReaderT GEOSHandle IO a }
  deriving (MonadReader GEOSHandle, Monad, Functor, Applicative)
-- don't derive MonadIO to restrict user from performing arbitrary IO


makeMessageHandler :: (String -> IO ()) -> IO (FunPtr I.GEOSMessageHandler)
makeMessageHandler f =  I.mkMessageHandler (\cs -> peekCString cs >>= f) 
 
-- not exposed
withGeos :: (I.GEOSContextHandle_t -> IO a) -> Geos a
withGeos f =  do
  {-mv <- asks unGEOSHandle-}
  Geos . ReaderT $ \gh -> MV.withMVar (unGEOSHandle gh) $ \fp -> withForeignPtr fp f

newGeos :: a -> Geos a
newGeos = return
 
runGeos :: Geos a -> a
runGeos g = unsafePerformIO $ do
  notice <- makeMessageHandler putStrLn
  err <- makeMessageHandler error
  ptrC <- I.geos_initGEOS notice err    
  fptr <- newForeignPtr I.geos_finishGEOS ptrC
  mv <- MV.newMVar fptr
  v <- runReaderT (unGeos g) $ GEOSHandle mv
  freeHaskellFunPtr notice
  freeHaskellFunPtr err
  return v
  

throwIfZero :: (Eq a, Num a) => (a -> String) -> IO a -> IO a
throwIfZero f m = throwIf (\v -> v == 0) f m

mkErrorMessage :: Show a => String -> (a -> String) 
mkErrorMessage s = \n -> s  <> " has thrown an error:  " <> show n


