{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-} 

module Data.Geometry.Geos.Raw.Base (
    Geos
  , runGeos  
  , throwIfZero
  , withGeos
  , mkErrorMessage
) where

import qualified Data.Geometry.Geos.Raw.Internal as I
import Foreign
import Data.Monoid ((<>))
import System.IO.Unsafe
import qualified Control.Concurrent.MVar as MV
import Control.Monad.Reader

newtype GEOSHandle = GEOSHandle (MV.MVar (ForeignPtr I.GEOSContextHandle_HS))

newtype Geos a = Geos { unGeos :: ReaderT GEOSHandle IO a }
  deriving (MonadReader GEOSHandle, Monad, Functor, Applicative)
-- don't derive MonadIO to restrict user from performing arbitrary IO

withGeos :: (I.GEOSContextHandle_t -> IO a) -> Geos a
withGeos f = Geos . ReaderT $ \(GEOSHandle mv) -> MV.withMVar mv $ \fp -> withForeignPtr fp f

runGeos :: Geos a -> a
runGeos g = unsafePerformIO $ do
  ptrC <- I.geos_init
  fptr <- newForeignPtr I.geos_finish ptrC
  mv <- MV.newMVar fptr
  runReaderT (unGeos g) $ GEOSHandle mv
  

throwIfZero :: (Eq a, Num a) => (a -> String) -> IO a -> IO a
throwIfZero = throwIf ((==) 0)

mkErrorMessage :: Show a => String -> (a -> String) 
mkErrorMessage s = \n -> s  <> " has thrown an error:  " <> show n
