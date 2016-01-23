{-# LANGUAGE CPP #-}

import GEOS.Raw.Internal 
import GEOS.Raw.Base
import qualified GEOS.Raw.Geometry as RG
import Foreign.Marshal.Utils
import Foreign
import Foreign.C.Types

newtype STRTree = STRTree (ForeignPtr I.GEOSGeometry)
  deriving (Show, Eq)



#if GEOS_VERSION_MAJOR > 3 && GEOS_VERSION_MINOR > 4
createSTRTree :: Int -> Geos STRTree 
createSTRTree n = withGeos $ \h -> geos_STRTreeCreate h $ fromIntegral n
#endif   
