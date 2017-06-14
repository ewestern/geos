{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Data.Geometry.Geos.Raw.Geometery

Light wrappers around Geos functions. Must be run within the Geos monad.

-}
module Data.Geometry.Geos.Raw.Geometry (
    Geom (..)
  , GeomConst (..)
  , Geometry (..)
  , getSRID
  , setSRID
  , getTypeName
  , getTypeId
  , getCoordinateSequence
  , getNumCoordinates
  , getNumInteriorRings
  , getNumGeometries
  , getInteriorRingN
  , getGeometryN
  , getExteriorRing
  , createPoint
  , createLinearRing
  , createLineString
  , createPolygon
  , createMultiPoint
  , createMultiLineString
  , createMultiPolygon
  , createCollection
  , project
  , projectNormalized 
  , interpolate
  , interpolateNormalized
  , disjoint
  , touches
  , intersects
  , crosses
  , within
  , contains
  , overlaps
  , equals
  , equalsExact
  , covers
  , coveredBy
  -- Misc functions
  , area
  , geometryLength
  , distance
  , hausdorffDistance
  , nearestPoints
  , normalize
) where
import qualified Data.Geometry.Geos.Raw.Internal as I
import Data.Geometry.Geos.Raw.Base
import Data.Geometry.Geos.Raw.CoordSeq
import Foreign
import Foreign.C.Types
import Foreign.C.String

newtype Geom = Geom { 
  unGeom :: (ForeignPtr I.GEOSGeometry)
} 

newtype GeomConst = GeomConst ( Ptr I.GEOSGeometry)

class Geometry a where
    withGeometry :: a  -> (Ptr I.GEOSGeometry -> IO b ) -> IO b

instance Eq Geom where
  a == b = runGeos $ do
    sa <- getSRID a
    sb <- getSRID b
    ta <- getTypeId a
    tb <- getTypeId b
    if (sa == sb) && (ta == tb)
      then do
        csa <- getCoordinateSequence a
        csb <- getCoordinateSequence b
        return $ csa == csb
      else return False

instance Geometry Geom where
    withGeometry (Geom g) f = withForeignPtr g f

instance Geometry GeomConst where
    withGeometry (GeomConst p) f = f p


getSRID :: Geometry a => a -> Geos (Maybe Int) 
getSRID g = withGeos $ \h -> do
  s <- withGeometry g $ I.geos_GetSRID h
  case fromIntegral s of
    0 -> return Nothing 
    i -> return (Just i)
    
setSRID :: Geometry a => (Maybe Int) -> a -> Geos a
setSRID Nothing g = return g
setSRID (Just i) g = withGeos $ \h ->  do
  withGeometry g $ \gp -> I.geos_SetSRID h gp $ fromIntegral i
  return g
  

getTypeName :: Geometry a => a -> Geos String
getTypeName g = withGeos $ \h ->  do
  s <- throwIfNull "getType" $ 
        withGeometry g $ I.geos_GeomType h
  return  =<< peekCString s

getTypeId ::Geometry a => a -> Geos Int
getTypeId g = withGeos $ \h -> do
  i <- throwIfNeg (mkErrorMessage "getTypeId")  $
      withGeometry g $ I.geos_GeomTypeId h
  return $ fromIntegral i

getCoordinateSequence :: Geometry a => a -> Geos CoordSeq
getCoordinateSequence g = do
  csc <- getCoordinateSequence_ g
  withGeos $ \h -> do
    cloned <- throwIfNull  "cloneCoordinateSequence" $ 
                withCoordinateSequence csc $ I.geos_CoordSeqClone h
    fptr <- newForeignPtrEnv I.geos_CoordSeqDestroy h cloned
    return $ CoordSeq fptr

-- must not be destroyed directly
getCoordinateSequence_ :: Geometry a => a -> Geos CoordSeqConst
getCoordinateSequence_ g = withGeos $ \h ->  do
  ptr <- throwIfNull  "getCoordinateSequence" $ 
          withGeometry g $ I.geos_GetCoordSeq h 
  return $ CoordSeqConst ptr


getNum_ :: Geometry a
        => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO CInt)
        -> a
        -> Geos Int
getNum_ f g = withGeos $ \h ->  do
  i <- throwIfNeg (mkErrorMessage "getNumCoordinates")  $
      withGeometry g $ f h 
  return $ fromIntegral i

getNumCoordinates :: Geometry a => a -> Geos Int
getNumCoordinates = getNum_ I.geos_GetNumCoordinates 

---- Polygons
getNumInteriorRings :: Geometry a => a -> Geos Int
getNumInteriorRings = getNum_ I.geos_GetNumInteriorRings

--- multi geometries
getNumGeometries :: Geometry a => a -> Geos Int
getNumGeometries = getNum_ I.geos_GetNumGeometries

getN_ :: Geometry a 
      => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> CInt -> IO (Ptr I.GEOSGeometry)) 
      -> a
      -> Int 
      -> Geos GeomConst
getN_ f g i = withGeos $ \h ->  do
  g' <- throwIfNull "getN" $ 
        withGeometry g $ \gp ->
          f h gp $ fromIntegral i 
  return $ GeomConst g'


getGeometryN_ :: Geometry a => a -> Int -> Geos GeomConst
getGeometryN_ = getN_ I.geos_GetGeometryN

getGeometryN :: Geometry a => a -> Int -> Geos Geom
getGeometryN g i = do
  cloned <- cloneConstGeometry =<< getGeometryN_ g i
  withGeos $ \h -> do
    fptr <- withGeometry cloned $ newForeignPtrEnv I.geos_GeomDestroy h
    return $ Geom fptr

-- must not be destroyed directly
getExteriorRing_ :: Geometry a => a -> Geos GeomConst
getExteriorRing_  g = withGeos $ \h ->  do
  r <- throwIfNull "getExteriorRing" $ 
        withGeometry g $ I.geos_GetExteriorRing h
  return $ GeomConst r


getExteriorRing :: Geometry a => a -> Geos Geom
getExteriorRing g = do
  nr <- cloneConstGeometry =<< getExteriorRing_ g 
  withGeos $ \h ->  do
    fptr <- withGeometry nr $ newForeignPtrEnv I.geos_GeomDestroy h
    return $ Geom fptr


-- must not be destroyed directly
getInteriorRingN_ :: Geometry a => a -> Int -> Geos GeomConst
getInteriorRingN_  = getN_ I.geos_GetInteriorRingN

getInteriorRingN :: Geometry a => a -> Int -> Geos Geom
getInteriorRingN g i = do
  nr <- cloneConstGeometry =<< getInteriorRingN_ g i
  withGeos $ \h -> do
    fptr <- withGeometry nr $ newForeignPtrEnv I.geos_GeomDestroy h
    return $ Geom fptr

normalize :: Geometry a => a -> Geos Geom
normalize g = withGeos $ \h -> do
  cloned <- withGeometry g $ I.geos_GeomClone h
  fp <- Geom <$> newForeignPtrEnv I.geos_GeomDestroy h cloned
  _ <- throwIfNeg (mkErrorMessage "normalize") $ withGeometry fp $ I.geos_Normalize h
  return fp
  
-- 

cloneConstGeometry :: GeomConst -> Geos Geom
cloneConstGeometry g = withGeos $ \h -> do
  gp <- withGeometry g $ I.geos_GeomClone h
  fp <- newForeignPtrEnv I.geos_GeomDestroy h gp
  return $ Geom fp
  
 {-Geometry Constructors.-}
 {-GEOSCoordSequence* arguments will become ownership of the returned object.-}
 {-All functions return NULL on exception.-}

createGeometry_ :: CoordinateSequence a 
                => (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> IO (Ptr I.GEOSGeometry)) 
                -> a 
                -> Geos Geom
createGeometry_ f c  = withGeos $ \h ->  do
   g <- throwIfNull "createGeometry" $ withCoordinateSequence c $ \pcs -> do 
   -- todo: clone for now, think of a better solution later
     cloned <- throwIfNull  "cloneCoordinateSequence" $ I.geos_CoordSeqClone h pcs
     f h cloned
   fp <- newForeignPtrEnv I.geos_GeomDestroy h g
   return $ Geom fp

-- Geometry Constructors
createPoint :: CoordinateSequence a => a -> Geos Geom
createPoint = createGeometry_ I.geos_GeomCreatePoint

createLinearRing :: CoordinateSequence a => a -> Geos Geom
createLinearRing = createGeometry_ I.geos_GeomCreateLinearRing

createLineString :: CoordinateSequence a => a -> Geos Geom
createLineString = createGeometry_ I.geos_GeomCreateLineString

-- TODO: Make this take a vector argument

-- | The second argument is a list of geometries,
-- | NOTE. geometries become owned by caller.
createPolygon :: Geometry a => a -> [a] -> Geos Geom
createPolygon o hs = withGeos $ \h -> do
  ptrs <- mapM (\v -> withGeometry v $ return) hs
  g <- withGeometry o $ \op -> 
        withArray ptrs $ \ph -> 
          I.geos_GeomCreatePolygon h op ph $ fromIntegral $ length hs
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g
  return $ Geom fp

createMulti_ :: Geometry a => I.GEOSGeomType -> [a] -> Geos Geom
createMulti_ t gs = withGeos $ \h -> do
  ptrs <- mapM (\v -> withGeometry v $ return) gs
  g <- withArray ptrs $ \ph ->
    I.geos_GeomCreateCollection h (I.unGEOSGeomType t) ph $ fromIntegral $ length gs
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g
  return $ Geom fp

createMultiPoint :: Geometry a => [a] -> Geos Geom
createMultiPoint = createMulti_ I.multiPointId 

createMultiLineString :: Geometry a => [a] -> Geos Geom
createMultiLineString = createMulti_  I.multiLineStringId

createMultiPolygon :: Geometry a => [a] -> Geos Geom
createMultiPolygon = createMulti_ I.multiPolygonId

createCollection :: Geometry a => [a] -> Geos Geom
createCollection = createMulti_ I.geometryCollectionId
 

        
--- Linear Referencing
----------------------
geo_2_ :: Geometry a 
        => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO CDouble)
        -> a 
        -> a
        -> Geos Double
geo_2_ f g p = withGeos $ \h -> do
   d <- withGeometry g $ \gp ->
          withGeometry p $ f h gp 
   return . realToFrac $ d

-- | @project p g@ returns the distance of point @p@ projected on @g@ from origin of @g@. Geometry @g@ must be a lineal geometry 
--
project :: Geometry a => a -> a -> Geos Double
project = geo_2_ I.geos_Project

projectNormalized :: Geometry a => a -> a -> Geos Double
projectNormalized = geo_2_ I.geos_ProjectNormalized


geo_1_d :: Geometry a
          => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> CDouble -> IO (Ptr I.GEOSGeometry))
          -> a
          -> Double
          -> Geos Geom
geo_1_d f g d = withGeos $ \h -> do
  g' <- withGeometry g $ \gp ->   
           f h gp $ realToFrac d
  fptr <- newForeignPtrEnv I.geos_GeomDestroy h g'
  return $ Geom fptr

-- | Return the closest point to given distance within geometry. Geometry must be a LineString 
--
interpolate :: Geometry a => a -> Double -> Geos Geom
interpolate = geo_1_d  I.geos_Interpolate 

interpolateNormalized :: Geometry a => a -> Double -> Geos Geom
interpolateNormalized = geo_1_d I.geos_InterpolateNormalized

--Binary Predicates
--------------------
binaryPredicate_ :: Geometry a
                  => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO CChar)
                  -> String
                  -> a
                  -> a
                  -> Geos Bool
binaryPredicate_ f s g1 g2 = withGeos $ \h -> do
  b <- throwIf ((==) 2) (mkErrorMessage s) $ 
        withGeometry g1 $ \gp1 ->
          withGeometry g2 $ f h gp1
  return . toBool $  b

disjoint :: Geometry a => a-> a -> Geos Bool
disjoint = binaryPredicate_ I.geos_Disjoint "disjoint"

touches :: Geometry a => a -> a -> Geos Bool
touches = binaryPredicate_ I.geos_Touches "touches"

intersects :: Geometry a => a -> a -> Geos Bool
intersects = binaryPredicate_ I.geos_Intersects "intersects"

crosses :: Geometry a => a -> a -> Geos Bool
crosses = binaryPredicate_ I.geos_Crosses "crosses"

within :: Geometry a => a -> a -> Geos Bool
within = binaryPredicate_ I.geos_Within "within"

contains :: Geometry a => a -> a -> Geos Bool
contains = binaryPredicate_ I.geos_Contains "contains"

overlaps :: Geometry a => a -> a -> Geos Bool
overlaps = binaryPredicate_ I.geos_Overlaps "overlaps"

equals :: Geometry a => a -> a -> Geos Bool
equals = binaryPredicate_ I.geos_Equals "equals" 

equalsExact :: Geometry a => a -> a -> Double -> Geos Bool
equalsExact g1' g2' d = binaryPredicate_ (\h g1 g2 -> I.geos_EqualsExact h g1 g2 (realToFrac d)) "equalsExact" g1' g2'

covers :: Geometry a => a -> a -> Geos Bool
covers = binaryPredicate_ I.geos_Covers "covers"

coveredBy :: Geometry a => a -> a -> Geos Bool
coveredBy = binaryPredicate_ I.geos_CoveredBy "coveredBy"

-- Misc functions

geo_1 ::  Geometry a 
      => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr CDouble -> IO CInt) 
      -> a
      -> Geos Double
geo_1 f g = withGeos $ \h -> alloca $ \dptr -> do
    _ <- throwIfZero (mkErrorMessage "geo_1" ) $ withGeometry g $ \gp -> 
        f h gp dptr 
    s <- peek dptr
    return $ realToFrac s

area :: Geometry a => a -> Geos Double
area = geo_1 I.geos_Area

geometryLength :: Geometry a => a -> Geos Double
geometryLength = geo_1 I.geos_Length

geo_2_d :: Geometry a
          => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> Ptr CDouble -> IO CInt)
          -> a
          -> a
          -> Geos Double
geo_2_d f g p = withGeos $ \h -> alloca $ \dptr -> do
   d <- throwIfZero (mkErrorMessage "geo_2") $ withGeometry g $ \gp ->
          withGeometry p $ \pp ->
               f h gp pp dptr
   return $ realToFrac  d

distance :: Geometry a => a -> a -> Geos Double
distance = geo_2_d I.geos_Distance

hausdorffDistance :: Geometry a => a -> a -> Geos Double
hausdorffDistance = geo_2_d I.geos_HausdorffDistance

nearestPoints :: Geometry a => a -> a -> Geos CoordSeq
nearestPoints g p = withGeos $ \h -> do
  ptr <-  withGeometry g $ \gp ->
            withGeometry p $ I.geos_NearestPoints h gp
  fptr <- newForeignPtrEnv I.geos_CoordSeqDestroy h ptr
  return $ CoordSeq fptr
