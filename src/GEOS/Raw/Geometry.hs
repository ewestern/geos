module GEOS.Raw.Geometry where
import qualified GEOS.Raw.Internal as I
import GEOS.Raw.Base
import GEOS.Raw.CoordSeq
import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr
import Data.Monoid ((<>))

import System.IO.Unsafe

newtype Geometry = Geometry { 
  _unGeometry :: (ForeignPtr I.GEOSGeometry)
}

newtype GeomConst = GeomConst {
  _unGeomConst :: Ptr I.GEOSGeometry
}

convertToBool :: (Integral a, Eq a)  => a -> Bool
convertToBool i = case fromIntegral i of
  0 -> False
  1 -> True 

withGeometry :: Geometry -> (Ptr I.GEOSGeometry -> IO a ) -> IO a
withGeometry (Geometry g) f = withForeignPtr g f

withGeomConst :: GeomConst -> (Ptr I.GEOSGeometry -> IO a) -> IO a
withGeomConst (GeomConst p) f = f p

getSRID :: GEOSHandle -> Geometry -> Int
getSRID h g = unsafePerformIO $ do
  s <- throwIfZero (mkErrorMessage "getSRID")  $
        withHandle h $ \hp -> 
          withGeometry g $ \gp ->
            I.geos_GetSRID hp gp
  return $ fromIntegral s
    
setSRID :: GEOSHandle -> Geometry -> Int -> ()
setSRID h g i = unsafePerformIO $ withHandle h $ \hp -> 
                  withGeometry g $ \gp ->
                    I.geos_SetSRID hp gp $ fromIntegral i
  

getType :: GEOSHandle -> Geometry -> Int
getType h g = unsafePerformIO $ do
  i <- throwIfNull "getType" $ withHandle h $ \hp -> 
        withGeometry g $ \gp ->
          I.geos_GeomType hp gp
  return . fromIntegral =<< peek i

getTypeId :: GEOSHandle -> Geometry -> Int
getTypeId h g = unsafePerformIO $ do
  i <- throwIfNeg (mkErrorMessage "getTypeId")  $
    withHandle h $ \hp ->
      withGeometry g $ \gp ->
        I.geos_GeomTypeId hp gp
  return $ fromIntegral i

getCoordinateSequence :: GEOSHandle -> Geometry -> CoordinateSequence
getCoordinateSequence h g = unsafePerformIO $ do
  let csc = getCoordinateSequence_ h g
  cloned <- throwIfNull  "cloneCoordinateSequence" $ withHandle h $ \hp ->
              withCoordSeqConst csc $ \ptr -> 
                I.geos_CoordSeqClone hp ptr
  fptr <- withHandle h $ \ch -> newForeignPtrEnv I.geos_CoordSeqDestroy ch cloned
  return $ CoordinateSequence fptr

-- must not be destroyed directly
getCoordinateSequence_ :: GEOSHandle -> Geometry -> CoordSeqConst
getCoordinateSequence_ h g = unsafePerformIO $ do
  ptr <- throwIfNull  "getCoordinateSequence" $ withHandle h $ \hp ->
          withGeometry g $ \gp ->
            I.geos_GetCoordSeq hp gp
  return $ CoordSeqConst ptr


getNum_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO CInt)
            -> GEOSHandle
            -> Geometry
            -> Int
getNum_ f h g = unsafePerformIO $  do
  i <- throwIfNeg (mkErrorMessage "getNumCoordinates")  $
    withHandle h $ \hp ->
      withGeometry g $ \gp ->
        f hp gp
  return $ fromIntegral i

getNumCoordinates :: GEOSHandle -> Geometry -> Int
getNumCoordinates = getNum_ I.geos_GetNumCoordinates 

---- Polygons
getNumInteriorRings :: GEOSHandle -> Geometry -> Int
getNumInteriorRings = getNum_ I.geos_GetNumInteriorRings

--- multi geometries
getNumGeometries :: GEOSHandle -> Geometry -> Int
getNumGeometries = getNum_ I.geos_GetNumGeometries

getN_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> CInt -> IO (Ptr I.GEOSGeometry)) 
          -> GEOSHandle 
          -> Geometry 
          -> Int 
          -> GeomConst
getN_ f h g i = unsafePerformIO $ do
  g <- throwIfNull "getN" $ withHandle h $ \hp ->
        withGeometry g $ \gp ->
          f hp gp $ fromIntegral i 
  return $ GeomConst g


getGeometryN_ :: GEOSHandle -> Geometry -> Int -> GeomConst
getGeometryN_ = getN_ I.geos_GetGeometryN

getGeometryN :: GEOSHandle -> Geometry -> Int -> Geometry
getGeometryN h g i = unsafePerformIO $ do
  let ig = getGeometryN_ h g i
      cloned = cloneConstGeometry h ig
  fptr <- withHandle h $ \ch -> 
            withGeometry cloned $ \gp ->
              newForeignPtrEnv I.geos_GeomDestroy ch gp
  return $ Geometry fptr

-- must not be destroyed directly
getExteriorRing_ :: GEOSHandle -> Geometry -> GeomConst
getExteriorRing_  h g = unsafePerformIO $ do
  r <- throwIfNull "getExteriorRing" $ withHandle h $ \hp ->
        withGeometry g $ \gp ->
          I.geos_GetExteriorRing hp gp
  return $ GeomConst r


getExteriorRing :: GEOSHandle -> Geometry -> Geometry
getExteriorRing h g = unsafePerformIO $ do
  let rc = getExteriorRing_ h g 
      nr = cloneConstGeometry h rc 
  fptr <- withHandle h $ \ch -> 
            withGeometry nr $ \gp -> 
              newForeignPtrEnv I.geos_GeomDestroy ch gp
  return $ Geometry fptr


-- must not be destroyed directly
getInteriorRingN_ :: GEOSHandle -> Geometry -> Int -> GeomConst
getInteriorRingN_  = getN_ I.geos_GetInteriorRingN

getInteriorRingN :: GEOSHandle -> Geometry -> Int -> Geometry  
getInteriorRingN h g i = unsafePerformIO $ do
  let rc = getInteriorRingN_ h g i
      nr = cloneConstGeometry h rc 
  fptr <- withHandle h $ \ch -> 
            withGeometry nr $ \nrp -> 
              newForeignPtrEnv I.geos_GeomDestroy ch nrp
  return $ Geometry fptr



{-normalize :: GEOSHandle -> Geometry -> IO Geometry-}
{-normalize h g = do-}
  {-ng <- cloneGeometry h g-}
  {-i <- throwIfNeg (mkErrorMessage "normalize") $  withHandle h $ \hp ->-}
          {-withGeometry ng $ \gp ->-}
            {-I.geos_Normalize hp gp-}
  {-return ng-}
  
-- 

cloneConstGeometry :: GEOSHandle -> GeomConst -> Geometry
cloneConstGeometry h g = unsafePerformIO $ do
  gp <- withHandle h $ \hp ->
    withGeomConst g $ \gp ->
      I.geos_GeomClone hp gp
  fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch gp
  return $ Geometry fp
  


createGeometry_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> IO (Ptr I.GEOSGeometry)) 
    -> GEOSHandle 
    -> CoordinateSequence 
    -> Geometry
createGeometry_ f h c  = unsafePerformIO $ do
   g <- throwIfNull "createGeometry" $ withCoordinateSequence c $ \pcs -> withHandle h $ \ch -> f ch pcs
   fp <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
   return $ Geometry fp

-- Geometry Constructors
createPoint :: GEOSHandle -> CoordinateSequence -> Geometry
createPoint = createGeometry_ I.geos_GeomCreatePoint

createLinearRing :: GEOSHandle -> CoordinateSequence -> Geometry
createLinearRing = createGeometry_ I.geos_GeomCreateLinearRing

createLineString :: GEOSHandle -> CoordinateSequence -> Geometry
createLineString = createGeometry_ I.geos_GeomCreateLineString


--- Linear Referencing
--
--
geo_2_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO CDouble)
          -> GEOSHandle 
          -> Geometry
          -> Geometry
          -> Double
geo_2_ f h g p = unsafePerformIO $ do
   d <- withHandle h $ \hp ->
          withGeometry g $ \gp ->
            withGeometry p $ \pp ->
               f hp gp pp 
   return . realToFrac $ d

--
-- Return distance of point 'p' projected on 'g' from origin
-- of 'g'. Geometry 'g' must be a lineal geometry 
project :: GEOSHandle -> Geometry -> Geometry -> Double
project = geo_2_ I.geos_Project

projectNormalized :: GEOSHandle -> Geometry -> Geometry -> Double
projectNormalized = geo_2_ I.geos_ProjectNormalized


geo_1_d :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> CDouble -> IO (Ptr I.GEOSGeometry))
          -> GEOSHandle 
          -> Geometry
          -> Double
          -> Geometry
geo_1_d f h g d = unsafePerformIO $ do
  g <- withHandle h $ \hp ->
        withGeometry g $ \gp ->   
           f hp gp $ realToFrac d
  fptr <- withHandle h $ \ch -> newForeignPtrEnv I.geos_GeomDestroy ch g
  return $ Geometry fptr

--Return closest point to given distance within geometry
-- Geometry must be a LineString 
interpolate :: GEOSHandle -> Geometry -> Double -> Geometry 
interpolate = geo_1_d  I.geos_Interpolate 

interpolateNormalized :: GEOSHandle -> Geometry -> Double -> Geometry
interpolateNormalized = geo_1_d I.geos_InterpolateNormalized

--Binary Predicates
binaryPredicate_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO CChar)
                    -> String
                    -> GEOSHandle
                    -> Geometry
                    -> Geometry 
                    -> Bool
binaryPredicate_ f s h g1 g2 = unsafePerformIO $ do
  b <- throwIf (\v -> v == 2) (mkErrorMessage s) $ withHandle h $ \hp ->
        withGeometry g1 $ \gp1 ->
          withGeometry g2 $ \gp2 ->
            f hp gp1 gp2
  return . convertToBool $  b

disjoint :: GEOSHandle -> Geometry -> Geometry -> Bool
disjoint = binaryPredicate_ I.geos_Disjoint "disjoint"

touches :: GEOSHandle -> Geometry -> Geometry -> Bool
touches = binaryPredicate_ I.geos_Touches "touches"

crosses :: GEOSHandle -> Geometry -> Geometry -> Bool
crosses = binaryPredicate_ I.geos_Crosses "crosses"

within :: GEOSHandle -> Geometry -> Geometry -> Bool
within = binaryPredicate_ I.geos_Within "within"

contains :: GEOSHandle -> Geometry -> Geometry -> Bool
contains = binaryPredicate_ I.geos_Contains "contains"

overlaps :: GEOSHandle -> Geometry -> Geometry -> Bool
overlaps = binaryPredicate_ I.geos_Overlaps "overlaps"

equals :: GEOSHandle -> Geometry -> Geometry -> Bool
equals = binaryPredicate_ I.geos_Equals "equals" 

equalsExact :: GEOSHandle -> Geometry -> Geometry -> Bool
equalsExact = binaryPredicate_ I.geos_EqualsExact "equalsExact"

covers :: GEOSHandle -> Geometry -> Geometry -> Bool
covers = binaryPredicate_ I.geos_Covers "covers"

coveredBy :: GEOSHandle -> Geometry -> Geometry -> Bool
coveredBy = binaryPredicate_ I.geos_CoveredBy "coveredBy"


