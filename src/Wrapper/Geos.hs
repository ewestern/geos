module GEOS.Wrapper where
import qualified GEOS.Internal as I
import qualified Data.Text as T

import Foreign.C.String
import System.IO.Unsafe

newtype GEOSHandle = GEOSHandle { _unGEOSHandle :: I.GEOSContextHandle }

makeMessageHandler :: (String -> IO ()) -> GEOSMessageHandler
makeMessageHandler f = \cs -> 
{-createCoordinateSequence = unsafePerformIO $ do-}
  

