{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


{-|
Module      : Data.Geometry.Geos.Raw.Geometery

Light wrappers around Geos functions. Must be run within the Geos monad.

-}
module Data.Geometry.Geos.Raw.Geometry (
    Geom (..)
  , GeomConst (..)
  , Geometry (..)
  , GeomTypeId (..)
  , withMaybeGeometry
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
import Foreign.Ptr (nullPtr)
import Foreign.C.Types
import Foreign.C.String

{- | 
A Geom is a wrapper around the C data structure that has finalizers associated with it.
-}
newtype Geom = Geom (ForeignPtr I.GEOSGeometry)

{- |
A GeomConst is a wrapper around the C data structure that does *not* have finalizers attached to it. A typical use case for GemoConst is when retrieving a child geometry from a composite geometry. If the parent geometry has finalizers associated with it, we can not separately attempt to deallocate memory occupied by the child geometry.
-}
newtype GeomConst = GeomConst ( Ptr I.GEOSGeometry)

class Geometry a where
    type CoordSeqInput a

    withGeometry :: a  
                  -> (Ptr I.GEOSGeometry -> IO b ) 
                  -> IO b
    constructGeometry :: I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO a

instance Geometry Geom where
    type CoordSeqInput Geom = CoordSeqConst
    withGeometry (Geom g) f = withForeignPtr g f
    constructGeometry h geo =  do
      fptr <- newForeignPtrEnv I.geos_GeomDestroy h geo
      return $ Geom fptr

instance Geometry GeomConst where
    type CoordSeqInput GeomConst = CoordSeq
    withGeometry (GeomConst p) f = f p
    constructGeometry _ geo =  return $ GeomConst geo

withMaybeGeometry :: Geometry a => Maybe a -> (Ptr I.GEOSGeometry -> IO b) -> IO b
withMaybeGeometry mg f = case mg of
    Just g -> withGeometry g f
    Nothing -> f nullPtr

createGeometryFromCoords :: Geometry b
                          => (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> IO (Ptr I.GEOSGeometry))
                          -> CoordSeqConst
                          -> Geos b
createGeometryFromCoords f c = 
   withGeos $ \h ->
      withCoordinateSequence c $ \pcs -> do
        ptr <- f h pcs
        constructGeometry h ptr

geomEq :: (Eq ca, Geometry a, CoordSeqInput a ~ ca, CoordinateSequence ca) => a -> a -> Bool
geomEq a b = runGeos $ do
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


instance Eq Geom where
  a == b = geomEq a b

instance Eq GeomConst where
  a == b = geomEq a b

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

data GeomTypeId = PointTypeId | LineStringTypeId | LinearRingTypeId | PolygonTypeId
                | MultiPointTypeId | MultiLineStringTypeId | MultiPolygonTypeId | GeometryCollectionTypeId deriving (Eq,Show)

geomTypeId :: Integer -> GeomTypeId
geomTypeId 0 = PointTypeId
geomTypeId 1 = LineStringTypeId
geomTypeId 2 = LinearRingTypeId
geomTypeId 3 = PolygonTypeId
geomTypeId 4 = MultiPointTypeId
geomTypeId 5 = MultiLineStringTypeId
geomTypeId 6 = MultiPolygonTypeId
geomTypeId 7 = GeometryCollectionTypeId
geomTypeId i = error $ "Not a valid geometry type " ++ (show i)

getTypeName :: Geometry a => a -> Geos String
getTypeName g = withGeos $ \h ->  do
  s <- throwIfNull "getType" $
        withGeometry g $ I.geos_GeomType h
  return  =<< peekCString s

getTypeId ::Geometry a => a -> Geos GeomTypeId
getTypeId g = withGeos $ \h -> do
  i <- throwIfNeg (mkErrorMessage "getTypeId") $ withGeometry g $ I.geos_GeomTypeId h
  return $ geomTypeId (fromIntegral i)


getCoordinateSequence :: Geometry a => a -> Geos CoordSeq
getCoordinateSequence g = withGeos $ \h ->
    withGeometry g $ \gptr -> do
        cptr <- throwIfNull "getCoordinateSequence" $ I.geos_GetCoordSeq h gptr
        cptr' <- I.geos_CoordSeqClone h cptr
        fptr <- newForeignPtrEnv I.geos_CoordSeqDestroy h cptr'
        return $ CoordSeq fptr


getNum_ :: Geometry a
        => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO CInt)
        -> a
        -> Geos Int
getNum_ f g = withGeos $ \h ->
  withGeometry g $ \g' -> do
    i <- throwIfNeg (mkErrorMessage "getNumCoordinates") $ f h g'
    return $ fromIntegral i


getNumCoordinates :: Geometry a => a -> Geos Int
getNumCoordinates = getNum_ I.geos_GetNumCoordinates

---- Polygons
getNumInteriorRings :: Geometry a => a -> Geos Int
getNumInteriorRings = getNum_ I.geos_GetNumInteriorRings

--- multi geometries
-- Returned object is a pointer to internal storage: it must NOT be destroyed directly.
getNumGeometries :: Geometry a => a -> Geos Int
getNumGeometries = getNum_ I.geos_GetNumGeometries

getN_ :: Geometry a
      => (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> CInt -> IO (Ptr I.GEOSGeometry))
      -> a
      -> Int
      -> Geos GeomConst
getN_ f g i = 
  withGeos $ \h ->  withGeometry g $ \gp ->  do
      gp' <- throwIfNull "getN" $ f h gp $ fromIntegral i
      I.geos_GeomClone h gp' >>= constructGeometry h


getGeometryN :: Geometry a => a -> Int -> Geos GeomConst
getGeometryN = getN_ I.geos_GetGeometryN

-- Returned object is a pointer to internal storage: it must NOT be destroyed directly.
getExteriorRing :: Geometry a => a -> Geos GeomConst
getExteriorRing  g = do
  withGeos $ \h -> do
      withGeometry g $ \gp ->  do
        gp' <- throwIfNull "getExteriorRing" $ I.geos_GetExteriorRing h gp 
        I.geos_GeomClone h gp' >>= constructGeometry h
  

getInteriorRingN :: Geometry a => a -> Int -> Geos GeomConst
getInteriorRingN  = getN_ I.geos_GetInteriorRingN

normalize :: Geometry a => a -> Geos a
normalize g = do
  cloned <- cloneGeometry g
  withGeos $ \h -> do
    _ <- throwIfNeg (mkErrorMessage "normalize") $ withGeometry cloned $ I.geos_Normalize h
    return ()
  return cloned
--

cloneGeometry :: Geometry a => a -> Geos a
cloneGeometry g = do
  withGeos $ \h -> withGeometry g $ \gp -> 
    I.geos_GeomClone h gp >>= constructGeometry h

-- Geometry Constructors
{-|
The following require CoordSeqConst as arguments since coordinate sequences become owned by the Geometry object.

-}
createPoint ::Geometry b => CoordSeqConst -> Geos b
createPoint = createGeometryFromCoords I.geos_GeomCreatePoint

createLinearRing :: Geometry a => CoordSeqConst -> Geos a
createLinearRing = createGeometryFromCoords I.geos_GeomCreateLinearRing

createLineString ::Geometry b => CoordSeqConst -> Geos b
createLineString = createGeometryFromCoords I.geos_GeomCreateLineString

-- | The second argument is a list of geometries,
-- | NOTE. geometries become owned by caller.
createPolygon :: Geometry a => GeomConst -> [GeomConst] -> Geos a
createPolygon o hs = do
  withGeos $ \h -> do
        ptrs <- mapM (\v -> withGeometry v $ return) hs
        withGeometry o $ \op -> do
          g' <- case ptrs of
                  [] -> I.geos_GeomCreatePolygon h op nullPtr 0
                  xs -> withArray xs $ \ph -> I.geos_GeomCreatePolygon h op ph $ fromIntegral $ length hs
          constructGeometry h g'


createMulti_ :: Geometry a => I.GEOSGeomType -> [GeomConst] -> Geos a
createMulti_ t gs = do
  withGeos $ \h -> do
      ptrs <- mapM (\v -> withGeometry v $ return) gs
      withArray ptrs $ \ph -> do
          g' <- I.geos_GeomCreateCollection h (I.unGEOSGeomType t) ph $ fromIntegral $ length gs
          constructGeometry h g'

createMultiPoint :: Geometry a =>  [GeomConst] -> Geos a
createMultiPoint = createMulti_ I.multiPointId

createMultiLineString :: Geometry a => [GeomConst] -> Geos a
createMultiLineString = createMulti_  I.multiLineStringId

createMultiPolygon :: Geometry a => [GeomConst] -> Geos a
createMultiPolygon = createMulti_ I.multiPolygonId

createCollection :: Geometry a => [GeomConst] -> Geos a
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
geo_1_d f g d = do
  withGeos $ \h -> withGeometry g $ \gp -> do
    gp' <- f h gp $ realToFrac d
    constructGeometry h gp'

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
    pure $ realToFrac s

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
   _ <- throwIfZero (mkErrorMessage "geo_2") $ withGeometry g $ \gp ->
          withGeometry p $ \pp ->
               f h gp pp dptr
   d <- peek dptr
   pure . realToFrac $ d

distance :: Geometry a => a -> a -> Geos Double
distance = geo_2_d I.geos_Distance

hausdorffDistance :: Geometry a => a -> a -> Geos Double
hausdorffDistance = geo_2_d I.geos_HausdorffDistance

nearestPoints :: (Geometry a, CoordinateSequence b) => a -> a -> Geos b
nearestPoints g p = do
  ptr <- withGeos $ \h -> withGeometry g $ \gp ->
            withGeometry p $ I.geos_NearestPoints h gp
  createCoordinateSequence ptr
