{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleContexts, TypeSynonymInstances, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-} 

module Data.Geometry.Geos.Raw.Base (
    Geos
  , runGeos  
  , runGeosE
  , runGeosM
  , throwIfZero
  , throwIfZero'
  , throwIfNull
  , throwIfNull'
  , throwIf
  , throwIf'
  , withGeos
  , withGeos'
  , mkErrorMessage
  , marshallInt
  , marshallDouble
  , geosUnit
) where

import qualified Data.Geometry.Geos.Raw.Internal as I
import Foreign hiding (throwIf, throwIfNull)
import Data.Monoid ((<>))
import System.IO.Unsafe
import qualified Control.Concurrent.MVar as MV
import Control.Monad.Reader
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Error.Class


newtype GEOSHandle = GEOSHandle (MV.MVar (ForeignPtr I.GEOSContextHandle_HS))


-- | Geos is the core context for performing geometric calculations. It handles the foreign function interface as well as error handling. All underlying geos exceptions are represented by a String. 
newtype Geos a = Geos { 
  unGeos :: ReaderT GEOSHandle (ExceptT String IO) a
}
  deriving (MonadReader GEOSHandle, Monad, Functor, Applicative)
-- don't derive MonadIO to restrict user from performing arbitrary IO
--

geosUnit :: Geos ()
geosUnit = pure ()

instance MonadError String Geos where
  throwError = Geos . lift . throwError
  catchError m f = Geos $ liftCatch catchError (unGeos m) (unGeos . f)

withGeos :: (I.GEOSContextHandle_t -> IO a) -> Geos a
withGeos f = Geos . ReaderT $ \(GEOSHandle mv) -> 
  ExceptT $  MV.withMVar mv $ \fp -> 
    Right <$> withForeignPtr fp f

withGeos' :: (I.GEOSContextHandle_t -> IO (Either String a)) -> Geos a
withGeos' f = Geos . ReaderT $ \(GEOSHandle mv) -> 
  ExceptT $  MV.withMVar mv $ \fp -> withForeignPtr fp f


-- | Unsafe function for performing a geos calculation. Errors will cause runtime exceptions.
runGeos :: Geos a -> a
runGeos action = case runGeosE action of
   Right v -> v
   Left e -> error e

-- | A safe function for performing a geos calculation and returning a Maybe. Exceptions will be represented as Nothing.
runGeosM :: Geos a -> Maybe a
runGeosM action = case runGeosE action of
   Right v -> Just v
   Left _ -> Nothing

-- | A safe function for performing a geos calculation and returning an Either String a. Exceptions are reprsented as `Left String`
runGeosE :: Geos a -> Either String a
runGeosE g = unsafePerformIO $ do
  ptrC <- I.geos_init
  fptr <- newForeignPtr I.geos_finish ptrC
  mv <- MV.newMVar fptr
  runExceptT $ runReaderT (unGeos g) $ GEOSHandle mv
  

throwIf :: (Eq a, MonadError e m) 
            => (a -> Bool) 
            -> (a -> e) 
            -> m a 
            -> m a
throwIf predicate mkError action = do
  val <- action
  if predicate val 
    then throwError $ mkError val
    else return val

throwIf' :: (Eq a, Monad m, MonadError e me ) 
            => (a -> Bool) 
            -> (a -> e) 
            -> m a 
            -> m (me a)
throwIf' predicate mkError action = do
  val <- action
  if predicate val 
    then return $ throwError  $ mkError val
    else return $ return  val
       
    

throwIfZero :: (Eq a, Num a, MonadError e m ) 
            => (a -> e) 
            -> m a 
            -> m a
throwIfZero = throwIf (0 ==)



throwIfZero' :: (Eq a, Num a, Monad m ) 
            => (a -> e) 
            -> m a 
            -> m (Either e a)
throwIfZero' = throwIf' (0 ==)



throwIfNull :: MonadError String m
          => String
          -> m (Ptr a)
          -> m (Ptr a)
throwIfNull location = throwIf (nullPtr ==) (\_ -> "Encountered null pointer at: " <> location)

throwIfNull' :: Monad m
          => String
          -> m (Ptr a)
          -> m (Either String (Ptr a))
throwIfNull' location = throwIf' (nullPtr ==) (\_ -> "Encountered null pointer at: " <> location)


marshallDouble :: (Real r, Storable r) => Ptr r -> IO Double
marshallDouble = fmap realToFrac . peek

         
marshallInt :: (Integral i, Storable i) => Ptr i -> IO Int
marshallInt = fmap fromIntegral . peek

mkErrorMessage :: Show a => String -> (a -> String) 
mkErrorMessage s n = s  <> " has thrown an error:  " <> show n
