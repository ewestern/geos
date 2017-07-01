import Data.Geometry.Geos.Geometry
import Data.Geometry.Geos.Types
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS8
import Data.Geometry.Geos.Serialize


loadThingsFromFile :: FilePath -> IO [Some Geometry]
loadThingsFromFile fp = do
  rows <- BS8.readFile fp
  return $ readHex <$> (BS8.lines rows)



main = do
    points <- (fmap ensurePoint) <$> loadThingsFromFile "tests/sampledata/points.csv"
    mapM_ print points

