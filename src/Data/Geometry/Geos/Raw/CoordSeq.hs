{-# LANGUAGE FlexibleInstances #-}

module Data.Geometry.Geos.Raw.CoordSeq (
    CoordinateSequence (..)
  , CoordSeqConst (CoordSeqConst)
  , CoordSeq (CoordSeq)
  , getCoordinateSequenceX
  , getCoordinateSequenceY
  , getCoordinateSequenceZ
  , getCoordinateSequenceSize
  , getCoordinateSequenceDimensions
  , setCoordinateSequenceX
  , setCoordinateSequenceY
  , setCoordinateSequenceZ
  , setCoordinateSequenceOrd
) where

import qualified Data.Geometry.Geos.Raw.Internal as I
import Data.Geometry.Geos.Raw.Base
import Foreign
import Foreign.C.Types
import Control.Monad

class CoordinateSequence a where
  withCoordinateSequence :: a -> (Ptr I.GEOSCoordSequence -> IO b) -> IO b
  createEmptyCoordinateSequence :: Int -> Int -> Geos a
  createCoordinateSequence :: Ptr I.GEOSCoordSequence -> Geos a

  

newtype CoordSeq = CoordSeq { 
  _unCoordSeq :: ForeignPtr I.GEOSCoordSequence
}
newtype CoordSeqConst = CoordSeqConst {
  _unCoordSeqConst :: Ptr I.GEOSCoordSequence
}

instance Eq CoordSeq where
  (==) = coordSeqEq

instance Eq CoordSeqConst where
  (==) = coordSeqEq

coordSeqEq :: CoordinateSequence a => a -> a -> Bool
coordSeqEq a b = runGeos $ do
  sa <- getCoordinateSequenceSize a
  sb <- getCoordinateSequenceSize b
  da <- getCoordinateSequenceDimensions a
  db <- getCoordinateSequenceDimensions b
  if (sa == sb) && (da == db)
      then foldM (comp (da == 3)) True [0..(sa-1)]
      else return False
  where
    comp zdim acc i = do
      xa <- getCoordinateSequenceX a i
      ya <- getCoordinateSequenceY a i
      xb <- getCoordinateSequenceX b i
      yb <- getCoordinateSequenceY b i
      zd <- if zdim
            then do
              za <- getCoordinateSequenceZ a i
              zb <- getCoordinateSequenceZ b i
              return $ za == zb
             else return True
      return $ (xa == xb)  && (ya == yb) && acc && zd

instance Show CoordSeq where
  show = coordSeqShow

instance Show CoordSeqConst where
  show = coordSeqShow

coordSeqShow :: CoordinateSequence a => a -> String
coordSeqShow a = runGeos $ do
  sa <- getCoordinateSequenceSize a
  unlines `fmap` mapM show' [0..(sa-1)]
  where
    show' i = do
      xa <- getCoordinateSequenceX a i
      ya <- getCoordinateSequenceY a i
      return . show $ (xa, ya)
    

instance CoordinateSequence CoordSeq where
  withCoordinateSequence (CoordSeq fp) f = withForeignPtr fp f
  createEmptyCoordinateSequence size dim = do
    ptr <- withGeos $ \h ->
      throwIfNull "createEmptyCoordinateSequence" $ I.geos_CoordSeqCreate h (fromIntegral size) (fromIntegral dim)
    createCoordinateSequence ptr

  createCoordinateSequence ptr = withGeos $ \h -> do
      fptr <- newForeignPtrEnv I.geos_CoordSeqDestroy h ptr
      return $ CoordSeq fptr

instance CoordinateSequence CoordSeqConst where
  withCoordinateSequence (CoordSeqConst p) f = f p
  createEmptyCoordinateSequence size dim = do
    ptr <- withGeos $ \h ->
      throwIfNull "createEmptyCoordinateSequence" $ I.geos_CoordSeqCreate h (fromIntegral size) (fromIntegral dim)
    createCoordinateSequence ptr
  createCoordinateSequence ptr = return $ CoordSeqConst ptr




getCoordinateSequenceD_ :: CoordinateSequence a 
                          => (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> Ptr CDouble -> IO CInt) 
                          -> a
                          -> Int
                          -> Geos Double 
getCoordinateSequenceD_ f cs idx = withGeos $ \h -> 
  alloca $ \dptr -> do
    _ <- throwIfZero (mkErrorMessage "getCoordiniateSequenceN") $
          withCoordinateSequence cs $ \pcs -> f h pcs (fromIntegral idx) dptr
    d <- peek dptr
    return $ realToFrac d
  
getCoordinateSequenceX :: CoordinateSequence a => a -> Int -> Geos Double
getCoordinateSequenceX = getCoordinateSequenceD_ I.geos_CoordSeqGetX

getCoordinateSequenceY :: CoordinateSequence a => a -> Int -> Geos Double
getCoordinateSequenceY = getCoordinateSequenceD_ I.geos_CoordSeqGetY

getCoordinateSequenceZ :: CoordinateSequence a => a -> Int -> Geos Double
getCoordinateSequenceZ = getCoordinateSequenceD_ I.geos_CoordSeqGetZ

getCoordinateSequenceSize :: CoordinateSequence a => a -> Geos Int 
getCoordinateSequenceSize c =  withGeos $ \h -> 
  alloca $ \ptr -> do
    _ <- throwIfZero (mkErrorMessage "getCoordinateSequenceSize") $ 
          withCoordinateSequence c $ \pc ->
            I.geos_CoordSeqGetSize h pc ptr
    s <- peek ptr
    return $ fromIntegral s

getCoordinateSequenceDimensions :: CoordinateSequence a => a -> Geos Int 
getCoordinateSequenceDimensions c = withGeos $ \h -> 
  alloca $ \ptr -> do
    _ <- throwIfZero (mkErrorMessage "getCoordinateSeqenceDimensions") $ 
            withCoordinateSequence c $ \pc ->
              I.geos_CoordSeqGetDimensions h pc ptr
    s <- peek ptr
    return $ fromIntegral s

---
setCoordinateSequence_ ::  CoordinateSequence a 
                        => (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> CUInt -> CDouble -> IO CInt) 
                        -> a 
                        -> Int 
                        -> Double 
                        -> Geos ()
setCoordinateSequence_ f cs idx val = withGeos $ \h -> do
  _ <- throwIfZero (mkErrorMessage "setCoordinateSEquenceN") $ 
        withCoordinateSequence cs $ \pcs -> 
          f h pcs (fromIntegral idx) (realToFrac val)
  return  ()


setCoordinateSequenceX :: CoordinateSequence a => a -> Int -> Double -> Geos ()
setCoordinateSequenceX = setCoordinateSequence_ I.geos_CoordSeqSetX

setCoordinateSequenceY :: CoordinateSequence a => a -> Int -> Double -> Geos ()
setCoordinateSequenceY = setCoordinateSequence_ I.geos_CoordSeqSetY

setCoordinateSequenceZ :: CoordinateSequence a => a -> Int -> Double -> Geos () 
setCoordinateSequenceZ = setCoordinateSequence_ I.geos_CoordSeqSetZ

setCoordinateSequenceOrd :: CoordinateSequence a => a -> Int -> Int  -> Double -> Geos Int
setCoordinateSequenceOrd cs idx dim v = withGeos $ \h -> do
  i <- throwIfZero (mkErrorMessage "setCoordinateSequenceN") $ 
          withCoordinateSequence cs $ \pcs -> 
            I.geos_CoordSeqSetOrdinate h pcs (fromIntegral idx) (fromIntegral dim) (realToFrac v)
  return $ fromIntegral i
