module Data.Geometry.Geos.CSV where

import Data.Csv(FromField(parseField))

instance FromField (Some Geometry) where
  parseField s = pure $ readHex

-- instance FromJSON (Some Geometry)       where parseJSON = parseSomeGeometry
-- instance A.FromJSON (Geometry Polygon)    where parseJSON = (fmap ensurePolygon)    . parseSomeGeometry
-- instance A.FromJSON (Geometry Point)      where parseJSON = (fmap ensurePoint)      . parseSomeGeometry
-- instance A.FromJSON (Geometry LinearRing) where parseJSON = (fmap ensureLinearRing) . parseSomeGeometry
-- instance A.FromJSON (Geometry LineString) where parseJSON = (fmap ensureLineString) . parseSomeGeometry

-- parseSomeGeometry = withText "Some Geometry" $ \t -> return $ readHex (Text.encodeUtf8 t) 
