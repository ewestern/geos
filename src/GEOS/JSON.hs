{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}
import Data.Aeson.TH
import Data.Aeson
import GEOS.Types
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map as Map


instance ToJSON Coordinate where
  toJSON (Coordinate2 x y) = toJSON (x, y)
  toJSON (Coordinate3 x y z) = toJSON (x, y, z)

instance FromJSON Coordinate where
  parseJSON v = parse2 v <|> parse3 v
    where 
      parse2 = liftA (uncurry Coordinate2) .  parseJSON 
      parse3 = liftA (\(x, y, z) -> Coordinate3 x y z) .  parseJSON 
 
instance ToJSON Point where
  toJSON (Point c) = toJSON $ Map.fromList 
      ["type" .= ("Point" :: T.Text)
      , "coordinates" .= toJSON c]

instance FromJSON Point where
  parseJSON (Object v) = do
    "Point" :: String <- v .: "type"
    c <- v .: "coordinates" 
    return $ Point c

  
  {-withObject "FromJSON Point" $ \v -> do-}
    {-p  <- v .: "type"-}
    {-if p == "Point" -}
      {-then do-}
        {-c <- v .: "coordinates" -}
        {-return $ Point c-}
      {-else fail "Invalid key"-}


{-instance ToJSON Geometry where-}
  {-toJSON (PointGeometry v s) = toJSON v-}
  {-toJSON (LineStringGeometry v s) = toJSON v-}
  {-toJSON (PolygonGeometry v s) = toJSON v-}
  {-toJSON (MultiPointGeometry v s) = toJSON v-}
  {-toJSON (MultiLineStringGeometry v s) = toJSON v-}
  {-toJSON (MultiPolygonGeometry v s) = toJSON v-}

{-instance FromJSON Geometry where-}
  {-parseJSON v = -}


{-deriveGeoJSON :: Name -> Q [Dec]-}
{-deriveGeoJSON n = derviceToJSON n <> deriveFromJSON n-}


deriveToJSON :: Name -> Q Dec
deriveToJSON n = [d| instance ToJSON $(contT n) where
                    toJSON $(patt) = toJSON $ Map.fromList 
                    ["type" A..= nameBase n
                    , "coordinates" A..= toJSON $(some)]]
    where
      patt = do
        n1 <- newName "a"
        ConP n [n1]



makeGeoCoordinates :: Name
makeGeoCoordinates n = do
   

{-asdf n = do-}
  {-TyConI (NewTypeD _ _ tvbs con _ ) <- reify e-}
  

  
