{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Data.Geometry.Geos.Raw.STRTree where

import Prelude hiding (foldr)
import qualified Data.Geometry.Geos.Raw.Internal as I
import qualified Data.Geometry.Geos.Raw.Geometry as RG
import qualified Data.Geometry.Geos.Raw.CoordSeq as RC
import Data.Geometry.Geos.Raw.Base
import Foreign
import qualified Foreign.Concurrent as FC
import Data.IORef
import qualified Data.Vector as V

--A query-only R-tree created using the Sort-Tile-Recursive (STR) algorithm. For two-dimensional spatial data.

--The STR packed R-tree is simple to implement and maximizes space utilization; that is, as many leaves as possible are filled to capacity. Overlap between nodes is far less than in a basic R-tree. However, once the tree has been built (explicitly or on the first call to query), items may not be added or removed.

class STRTreeLike t where
    withSTRTree :: t a -> (Ptr I.GEOSSTRTree -> IO b ) -> IO b

instance STRTreeLike STRTree where
    withSTRTree (STRTree t) f = withForeignPtr t f
       
instance STRTreeLike STRTreeBuilder where
    withSTRTree (STRTreeBuilder t) f = withForeignPtr t f

{-|
  A query-only data structure
-}
newtype STRTree a = STRTree (ForeignPtr I.GEOSSTRTree)
  deriving (Show, Eq)

{-|
  A mutable data structure. Must `build` into an STRTree before querying.
-}
newtype STRTreeBuilder a = STRTreeBuilder (ForeignPtr I.GEOSSTRTree)
  deriving (Show, Eq)

type Finalizer a b = Ptr a -> Ptr b -> IO ()

foreign import ccall "wrapper"
  wrap2 :: (Ptr a -> Ptr () -> IO ()) -> IO (I.GEOSQueryCallback a)

foreign import ccall "dynamic"
  unwrap :: FunPtr (Finalizer a b) -> Finalizer a b 
  

{- |
Create a new R-tree using the Sort-Tile-Recursive algorithm (STRtree) for two-dimensional spatial data.

 @param nodeCapacity the maximum number of child nodes that a node may have.  The minimum recommended capacity value is 4.  If unsure, use a default node capacity of 10.
-}
createSTRTreeBuilder :: Int -> Geos (STRTreeBuilder a)
createSTRTreeBuilder nodeCapacity = withGeos $ \h -> do
    t <- I.geos_STRTreeCreate h $ fromIntegral nodeCapacity
    let cleanup = unwrap I.geos_STRTreeDestroy h t
    fp <- FC.newForeignPtr t cleanup
    return $ STRTreeBuilder fp


{- |
  "Builds" the STRTree by performing a trivial query on it.
-}
build :: STRTreeBuilder a -> Geos (STRTree a)
build tree@(STRTreeBuilder fp) = do 
  cs <- RC.createEmptyCoordinateSequence 1 2
  p ::RG.Geom <- RG.createPoint cs
  withGeos $ \h -> do
      callback <- wrap2 $ \_ _ -> return ()
      RG.withGeometry p $ \rg ->
          withSTRTree tree $ \st -> I.geos_STRTreeQuery h st rg callback nullPtr
  return $ STRTree fp
  
{-|
Insert an item into an STRtree

@param tree the STRtree in which the item should be inserted
@param geometry a Geometry whose envelope corresponds to the extent of 'item'
@param item the item to insert into the tree
-}
insert :: (Storable b, RG.Geometry a)  => STRTreeBuilder b -> a -> b -> Geos ()
insert (STRTreeBuilder tree) geometry item =
  withGeos $ \h -> do
    ptr <- malloc
    poke ptr item
    _ <- withForeignPtr tree $ \st -> do
        RG.withGeometry geometry $ \gr ->
          I.geos_STRTreeInsert h st gr ptr
    let cleanup = free ptr
    FC.addForeignPtrFinalizer tree cleanup
    return ()

foldr :: (STRTreeLike t, Storable a) => (a -> b -> b) -> b -> t a -> Geos b
foldr func acc tree = withGeos $ \h -> do
    b <- newIORef acc
    callback <- wrap2 $ \a _ -> do
      i <- peek a
      modifyIORef' b (func i)
    _ <- withSTRTree tree $ \st -> I.geos_STRTreeIterate h st callback nullPtr
    freeHaskellFunPtr callback
    readIORef b

query :: (Storable b, RG.Geometry a) 
      => STRTree b 
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

