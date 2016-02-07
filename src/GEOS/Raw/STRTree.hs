{-# LANGUAGE CPP #-}

module GEOS.Raw.STRTree where

import qualified GEOS.Raw.Internal as I
import qualified GEOS.Raw.Geometry as RG
import GEOS.Raw.Base
import Foreign
import Data.IORef
import qualified Data.Vector as V

--A query-only R-tree created using the Sort-Tile-Recursive (STR) algorithm. For two-dimensional spatial data.

--The STR packed R-tree is simple to implement and maximizes space utilization; that is, as many leaves as possible are filled to capacity. Overlap between nodes is far less than in a basic R-tree. However, once the tree has been built (explicitly or on the first call to query), items may not be added or removed.
newtype STRTree a = STRTree (ForeignPtr I.GEOSSTRTree)
  deriving (Show, Eq)

withSTRTree :: STRTree a -> (Ptr I.GEOSSTRTree -> IO b ) -> IO b
withSTRTree (STRTree t) f = withForeignPtr t f

--if GEOS_VERSION_MAJOR >= 3 && GEOS_VERSION_MINOR > 4
foreign import ccall "wrapper"
  wrap :: (Ptr () -> Ptr () -> IO ()) -> IO (I.GEOSQueryCallback)

createSTRTree :: Int -> Geos (STRTree a)
createSTRTree n = withGeos $ \h -> do
   t <- I.geos_STRTreeCreate h $ fromIntegral n
   fp <- newForeignPtrEnv I.geos_STRTreeDestroy h t
   return $ STRTree fp


insert :: Storable b => STRTree b -> RG.Geometry -> b -> Geos (STRTree b)
insert t g i =  
  withGeos $ \h -> alloca $ \ptr -> do
    poke ptr i
    _ <- withSTRTree t $ \st -> RG.withGeometry g $ \gr -> 
        I.geos_STRTreeInsert h st gr (castPtr ptr)
    return t

{-iterate :: Storable b => (STRTree b) -> Geos (V.Vector b)-}
{-iterate t = -}
  {-withGeos $ \h -> do-}
    {-r <- newIORef $ V.empty-}
    {-callback <- wrap $ \a _ -> do-}
      {-i <- peek $ castPtr a-}
      {-modifyIORef' r (\v -> V.snoc v i)-}
    {-_ <- withSTRTree t $ \st -> I.geos_STRTreeIterate h st callback nullPtr-}
    {-readIORef r-}


query :: Storable b => (STRTree b) -> RG.Geometry -> Geos (V.Vector b)
query t g  = 
  withGeos $ \h -> do
    r <- newIORef $ V.empty
    callback <- wrap $ \a _ -> do
      i <- peek $ castPtr a
      modifyIORef' r (\v -> V.snoc v i)
    _ <- RG.withGeometry g $ \rg ->  
          withSTRTree t $ \st -> I.geos_STRTreeQuery h st rg callback nullPtr
    readIORef r
--endif   

