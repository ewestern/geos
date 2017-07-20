{-# LANGUAGE CPP #-}



module Data.Geometry.Geos.Raw.STRTree where

import Prelude hiding (foldr)
import qualified Data.Geometry.Geos.Raw.Internal as I
import qualified Data.Geometry.Geos.Raw.Geometry as RG
import Data.Geometry.Geos.Raw.Base
import Foreign
import qualified Foreign.Concurrent as FC
import Data.IORef
import qualified Data.Vector as V

--A query-only R-tree created using the Sort-Tile-Recursive (STR) algorithm. For two-dimensional spatial data.

--The STR packed R-tree is simple to implement and maximizes space utilization; that is, as many leaves as possible are filled to capacity. Overlap between nodes is far less than in a basic R-tree. However, once the tree has been built (explicitly or on the first call to query), items may not be added or removed.

newtype STRTree a = STRTree (ForeignPtr I.GEOSSTRTree)
  deriving (Show, Eq)

withSTRTree :: STRTree a -> (Ptr I.GEOSSTRTree -> IO b ) -> IO b
withSTRTree (STRTree t) f = withForeignPtr t f

type Finalizer a b = Ptr a -> Ptr b -> IO ()

foreign import ccall "wrapper"
  wrap2 :: (Ptr a -> Ptr () -> IO ()) -> IO (I.GEOSQueryCallback a)

foreign import ccall "dynamic"
  unwrap :: FunPtr (Finalizer a b) -> Finalizer a b 
  

{- |
Create a new R-tree using the Sort-Tile-Recursive algorithm (STRtree) for two-dimensional spatial data.

 @param nodeCapacity the maximum number of child nodes that a node may have.  The minimum recommended capacity value is 4.  If unsure, use a default node capacity of 10.
-}

createSTRTree :: Int -> Geos (STRTree a)
createSTRTree nodeCapacity = withGeos $ \h -> do
    t <- I.geos_STRTreeCreate h $ fromIntegral nodeCapacity
    let cleanup = unwrap I.geos_STRTreeDestroy h t
    fp <- FC.newForeignPtr t cleanup
    return $ STRTree fp



{-|
Insert an item into an STRtree

@param tree the STRtree in which the item should be inserted
@param geometry a Geometry whose envelope corresponds to the extent of 'item'
@param item the item to insert into the tree
-}

insert :: (Storable b, RG.Geometry a)  => STRTree b -> a -> b -> Geos ()
insert (STRTree tree) geometry item =
  withGeos $ \h -> do
    ptr <- malloc
    poke ptr item
    _ <- withForeignPtr tree $ \st -> do
        RG.withGeometry geometry $ \gr ->
          I.geos_STRTreeInsert h st gr ptr
    let cleanup = free ptr
    FC.addForeignPtrFinalizer tree cleanup
    return ()

foldr :: Storable a => (a -> b -> b) -> b -> STRTree a -> Geos b
foldr func acc tree = withGeos $ \h -> do
    b <- newIORef acc
    callback <- wrap2 $ \a _ -> do
      i <- peek a
      modifyIORef' b (func i)
    _ <- withSTRTree tree $ \st -> I.geos_STRTreeIterate h st callback nullPtr
    freeHaskellFunPtr callback
    readIORef b

query :: (Storable b, RG.Geometry a) 
      => (STRTree b) 
      -> a
      -> Geos (V.Vector b)
query t g  =
  withGeos $ \h -> do
    r <- newIORef V.empty
    callback <- wrap2 $ \a _ -> do
      i <- peek a
      modifyIORef' r (flip V.snoc i)
    _ <- RG.withGeometry g $ \rg ->
          withSTRTree t $ \st -> 
            I.geos_STRTreeQuery h st rg callback nullPtr
    freeHaskellFunPtr callback
    readIORef r

