import Data.Aeson.TH

deriveGeoJSON :: Name -> Q [Dec]
deriveGeoJSON n = derviceToJSON n <> deriveFromJSON n


deriveToJSON :: Name -> Q Dec
deriveToJSON n = do
    [d| instance ToJSON $(contT n) where
          toJSON $(som) = toJSON $ Map.fromList 
          ["type" A..= ($(name) :: BS.ByteString)
          , "coordinates" A..= toJSON $(some)]]

asdf n = do
  TyConI (NewTypeD _ _ tvbs con _ ) <- reify e
  

  
