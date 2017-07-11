{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-} 

module Data.Geometry.Geos.Raw.Base (
    Geos
  , runGeos  
  , throwIfZero
  , withGeos
  , mkErrorMessage
  , (>><)
) where

import qualified Data.Geometry.Geos.Raw.Internal as I
import Foreign
import Data.Monoid ((<>))
import System.IO.Unsafe
import qualified Control.Concurrent.MVar as MV
import Control.Monad.Reader

infixl 1 >><
(>><) :: Monad m => m a -> (a -> m b) -> m a
m >>< f = do
  a <- m
  _ <- f a
  return a

newtype GEOSHandle = GEOSHandle { 
  unGEOSHandle :: MV.MVar (ForeignPtr I.GEOSContextHandle_HS)
}

newtype Geos a = Geos { unGeos :: ReaderT GEOSHandle IO a }
  deriving (MonadReader GEOSHandle, Monad, Functor, Applicative)
-- don't derive MonadIO to restrict user from performing arbitrary IO

withGeos :: (I.GEOSContextHandle_t -> IO a) -> Geos a
withGeos f = Geos . ReaderT $ \gh -> MV.withMVar (unGEOSHandle gh) $ \fp -> withForeignPtr fp f

runGeos :: Geos a -> a
runGeos g = unsafePerformIO $ do
  ptrC <- I.geos_init    
  fptr <- newForeignPtr I.geos_finish ptrC
  mv <- MV.newMVar fptr
  v <- runReaderT (unGeos g) $ GEOSHandle mv
  return v
  

throwIfZero :: (Eq a, Num a) => (a -> String) -> IO a -> IO a
throwIfZero = throwIf ((==) 0)

mkErrorMessage :: Show a => String -> (a -> String) 
mkErrorMessage s = \n -> s  <> " has thrown an error:  " <> show n


