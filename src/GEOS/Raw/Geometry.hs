module GEOS.Raw.Geometry (
    Geometry (..)
  , withGeometry
  , getSRID
  , setSRID
  , getType
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

) where
import qualified GEOS.Raw.Internal as I
import GEOS.Raw.Base
import GEOS.Raw.CoordSeq
import Foreign
import Foreign.C.Types

newtype Geometry = Geometry { 
  unGeometry :: (ForeignPtr I.GEOSGeometry)
} deriving (Show, Eq)

newtype GeomConst = GeomConst ( Ptr I.GEOSGeometry)
 deriving (Show, Eq)

{-withVector :: Storable a => V.Vector a -> (Ptr a -> IO b) -> IO b-}
{-withVector v f = -}
  {-allocaArray len $ \ptr -> do-}
    {-pokeArray ptr -}
  {-where-}
    {-len = V.length v-}

withGeometry :: Geometry -> (Ptr I.GEOSGeometry -> IO a ) -> IO a
withGeometry (Geometry g) f = withForeignPtr g f

withGeomConst :: GeomConst -> (Ptr I.GEOSGeometry -> IO a) -> IO a
withGeomConst (GeomConst p) f = f p

-- TODO: return Maybe Int
getSRID :: Geometry -> Geos (Maybe Int) 
getSRID g = withGeos $ \h -> do
  s <- withGeometry g $ \gp ->
            I.geos_GetSRID h gp
  case fromIntegral s of
    0 -> return Nothing 
    i -> return (Just i)
    
setSRID :: Geometry -> (Maybe Int) -> Geos ()
setSRID _ Nothing = return ()
setSRID g (Just i) = withGeos $ \h -> 
    withGeometry g $ \gp -> 
      I.geos_SetSRID h gp $ fromIntegral i
  

getType :: Geometry -> Geos Int
getType g = withGeos $ \h ->  do
  i <- throwIfNull "getType" $ 
        withGeometry g $ \gp ->
          I.geos_GeomType h gp
  return . fromIntegral =<< peek i

getTypeId ::Geometry -> Geos Int
getTypeId g = withGeos $ \h -> do
  i <- throwIfNeg (mkErrorMessage "getTypeId")  $
      withGeometry g $ \gp ->
        I.geos_GeomTypeId h gp
  return $ fromIntegral i

getCoordinateSequence :: Geometry -> Geos CoordinateSequence
getCoordinateSequence g = do
  csc <- getCoordinateSequence_ g
  withGeos $ \h -> do
    cloned <- throwIfNull  "cloneCoordinateSequence" $ 
                withCoordSeqConst csc $ \ptr -> 
                  I.geos_CoordSeqClone h ptr
    fptr <- newForeignPtrEnv I.geos_CoordSeqDestroy h cloned
    return $ CoordinateSequence fptr

-- must not be destroyed directly
getCoordinateSequence_ :: Geometry -> Geos CoordSeqConst
getCoordinateSequence_ g = withGeos $ \h ->  do
  ptr <- throwIfNull  "getCoordinateSequence" $ 
          withGeometry g $ \gp ->
            I.geos_GetCoordSeq h gp
  return $ CoordSeqConst ptr


getNum_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> IO CInt)
            -> Geometry
            -> Geos Int
getNum_ f g = withGeos $ \h ->  do
  i <- throwIfNeg (mkErrorMessage "getNumCoordinates")  $
      withGeometry g $ \gp ->
        f h gp
  return $ fromIntegral i

getNumCoordinates :: Geometry -> Geos Int
getNumCoordinates = getNum_ I.geos_GetNumCoordinates 

---- Polygons
getNumInteriorRings :: Geometry -> Geos Int
getNumInteriorRings = getNum_ I.geos_GetNumInteriorRings

--- multi geometries
getNumGeometries :: Geometry -> Geos Int
getNumGeometries = getNum_ I.geos_GetNumGeometries

getN_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> CInt -> IO (Ptr I.GEOSGeometry)) 
          -> Geometry 
          -> Int 
          -> Geos GeomConst
getN_ f g i = withGeos $ \h ->  do
  g' <- throwIfNull "getN" $ 
        withGeometry g $ \gp ->
          f h gp $ fromIntegral i 
  return $ GeomConst g'


getGeometryN_ :: Geometry -> Int -> Geos GeomConst
getGeometryN_ = getN_ I.geos_GetGeometryN

getGeometryN :: Geometry -> Int -> Geos Geometry
getGeometryN g i = do
  cloned <- cloneConstGeometry =<< getGeometryN_ g i
  withGeos $ \h -> do
    fptr <- withGeometry cloned $ \gp ->
                newForeignPtrEnv I.geos_GeomDestroy h gp
    return $ Geometry fptr

-- must not be destroyed directly
getExteriorRing_ :: Geometry -> Geos GeomConst
getExteriorRing_  g = withGeos $ \h ->  do
  r <- throwIfNull "getExteriorRing" $ 
        withGeometry g $ \gp ->
          I.geos_GetExteriorRing h gp
  return $ GeomConst r


getExteriorRing :: Geometry -> Geos Geometry
getExteriorRing g = do
  nr <- cloneConstGeometry =<< getExteriorRing_ g 
  withGeos $ \h ->  do
    fptr <- withGeometry nr $ \gp -> 
                newForeignPtrEnv I.geos_GeomDestroy h gp
    return $ Geometry fptr


-- must not be destroyed directly
getInteriorRingN_ :: Geometry -> Int -> Geos GeomConst
getInteriorRingN_  = getN_ I.geos_GetInteriorRingN

getInteriorRingN :: Geometry -> Int -> Geos Geometry  
getInteriorRingN g i = do
  nr <- cloneConstGeometry =<< getInteriorRingN_ g i
  withGeos $ \h -> do
    fptr <- withGeometry nr $ \nrp -> 
                newForeignPtrEnv I.geos_GeomDestroy h nrp
    return $ Geometry fptr



{-normalize :: GEOSHandle -> Geometry -> IO Geometry-}
{-normalize h g = do-}
  {-ng <- cloneGeometry h g-}
  {-i <- throwIfNeg (mkErrorMessage "normalize") $  withHandle h $ \hp ->-}
          {-withGeometry ng $ \gp ->-}
            {-I.geos_Normalize hp gp-}
  {-return ng-}
  
-- 

cloneConstGeometry :: GeomConst -> Geos Geometry
cloneConstGeometry g = withGeos $ \h -> do
  gp <- withGeomConst g $ \gp ->
      I.geos_GeomClone h gp
  fp <- newForeignPtrEnv I.geos_GeomDestroy h gp
  return $ Geometry fp
  
 {-Geometry Constructors.-}
 {-GEOSCoordSequence* arguments will become ownership of the returned object.-}
 {-All functions return NULL on exception.-}

createGeometry_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSCoordSequence -> IO (Ptr I.GEOSGeometry)) 
    -> CoordinateSequence 
    -> Geos Geometry
createGeometry_ f c  = withGeos $ \h ->  do
   g <- throwIfNull "createGeometry" $ withCoordinateSequence c $ \pcs -> do 
   -- todo: clone for now, think of a better solution later
     cloned <- throwIfNull  "cloneCoordinateSequence" $ I.geos_CoordSeqClone h pcs
     f h cloned
   fp <- newForeignPtrEnv I.geos_GeomDestroy h g
   return $ Geometry fp

-- Geometry Constructors
createPoint :: CoordinateSequence -> Geos Geometry
createPoint = createGeometry_ I.geos_GeomCreatePoint

createLinearRing :: CoordinateSequence -> Geos Geometry
createLinearRing = createGeometry_ I.geos_GeomCreateLinearRing

createLineString :: CoordinateSequence -> Geos Geometry
createLineString = createGeometry_ I.geos_GeomCreateLineString

-- TODO: Make this take a vector argument

-- | The second argument is a list of geometries,
-- | NOTE. geometries become owned by caller.
createPolygon :: Geometry -> [Geometry] -> Geos Geometry
createPolygon o hs = withGeos $ \h -> do
  ptrs <- mapM (\v -> withGeometry v $ return) hs
  g <- withGeometry o $ \op -> 
        withArray ptrs $ \ph -> 
          I.geos_GeomCreatePolygon h op ph $ fromIntegral $ length hs
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g
  return $ Geometry fp

createMulti_ :: I.GEOSGeomType -> [Geometry] -> Geos Geometry 
createMulti_ t gs = withGeos $ \h -> do
  ptrs <- mapM (\v -> withGeometry v $ return) gs
  g <- withArray ptrs $ \ph ->
    I.geos_GeomCreateCollection h (I.unGEOSGeomType t) ph $ fromIntegral $ length gs
  fp <- newForeignPtrEnv I.geos_GeomDestroy h g
  return $ Geometry fp

createMultiPoint :: [Geometry] -> Geos Geometry 
createMultiPoint = createMulti_ I.multiPointId 

createMultiLineString :: [Geometry] -> Geos Geometry
createMultiLineString = createMulti_  I.multiLineStringId

createMultiPolygon :: [Geometry] -> Geos Geometry
createMultiPolygon = createMulti_ I.multiPolygonId

createCollection :: [Geometry] -> Geos Geometry
createCollection = createMulti_ I.geometryCollectionId
 

        
--- Linear Referencing
----------------------
geo_2_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO CDouble)
          -> Geometry
          -> Geometry
          -> Geos Double
geo_2_ f g p = withGeos $ \h -> do
   d <- withGeometry g $ \gp ->
          withGeometry p $ \pp ->
               f h gp pp 
   return . realToFrac $ d

-- | @project p g@ returns the distance of point @p@ projected on @g@ from origin of @g@. Geometry @g@ must be a lineal geometry 
--
project :: Geometry -> Geometry -> Geos Double
project = geo_2_ I.geos_Project

projectNormalized :: Geometry -> Geometry -> Geos Double
projectNormalized = geo_2_ I.geos_ProjectNormalized


geo_1_d :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> CDouble -> IO (Ptr I.GEOSGeometry))
          -> Geometry
          -> Double
          -> Geos Geometry
geo_1_d f g d = withGeos $ \h -> do
  g' <- withGeometry g $ \gp ->   
           f h gp $ realToFrac d
  fptr <- newForeignPtrEnv I.geos_GeomDestroy h g'
  return $ Geometry fptr

-- | Return the closest point to given distance within geometry. Geometry must be a LineString 
--
interpolate :: Geometry -> Double -> Geos Geometry 
interpolate = geo_1_d  I.geos_Interpolate 

interpolateNormalized :: Geometry -> Double -> Geos Geometry
interpolateNormalized = geo_1_d I.geos_InterpolateNormalized

--Binary Predicates
--------------------
binaryPredicate_ :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> IO CChar)
                    -> String
                    -> Geometry
                    -> Geometry 
                    -> Geos Bool
binaryPredicate_ f s g1 g2 = withGeos $ \h -> do
  b <- throwIf (\v -> v == 2) (mkErrorMessage s) $ 
        withGeometry g1 $ \gp1 ->
          withGeometry g2 $ \gp2 ->
            f h gp1 gp2
  return . toBool $  b

disjoint :: Geometry -> Geometry -> Geos Bool
disjoint = binaryPredicate_ I.geos_Disjoint "disjoint"

touches :: Geometry -> Geometry -> Geos Bool
touches = binaryPredicate_ I.geos_Touches "touches"

crosses :: Geometry -> Geometry -> Geos Bool
crosses = binaryPredicate_ I.geos_Crosses "crosses"

within :: Geometry -> Geometry -> Geos Bool
within = binaryPredicate_ I.geos_Within "within"

contains :: Geometry -> Geometry -> Geos Bool
contains = binaryPredicate_ I.geos_Contains "contains"

overlaps :: Geometry -> Geometry -> Geos Bool
overlaps = binaryPredicate_ I.geos_Overlaps "overlaps"

equals :: Geometry -> Geometry -> Geos Bool
equals = binaryPredicate_ I.geos_Equals "equals" 

equalsExact :: Geometry -> Geometry -> Geos Bool
equalsExact = binaryPredicate_ I.geos_EqualsExact "equalsExact"

covers :: Geometry -> Geometry -> Geos Bool
covers = binaryPredicate_ I.geos_Covers "covers"

coveredBy :: Geometry -> Geometry -> Geos Bool
coveredBy = binaryPredicate_ I.geos_CoveredBy "coveredBy"

-- Misc functions

geo_1 :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr CDouble -> IO CInt) 
          -> Geometry
          -> Geos Double
geo_1 f g = withGeos $ \h -> alloca $ \dptr -> do
    _ <- throwIfZero (mkErrorMessage "geo_1" ) $ withGeometry g $ \gp -> 
        f h gp dptr 
    s <- peek dptr
    return $ realToFrac s

area :: Geometry -> Geos Double
area = geo_1 I.geos_Area

geometryLength :: Geometry -> Geos Double
geometryLength = geo_1 I.geos_Length

geo_2_d :: (I.GEOSContextHandle_t -> Ptr I.GEOSGeometry -> Ptr I.GEOSGeometry -> Ptr CDouble -> IO CInt)
          -> Geometry
          -> Geometry
          -> Geos Double
geo_2_d f g p = withGeos $ \h -> alloca $ \dptr -> do
   d <- throwIfZero (mkErrorMessage "geo_2") $ withGeometry g $ \gp ->
          withGeometry p $ \pp ->
               f h gp pp dptr
   return $ realToFrac  d

distance :: Geometry -> Geometry -> Geos Double
distance = geo_2_d I.geos_Distance

hausdorffDistance :: Geometry -> Geometry -> Geos Double
hausdorffDistance = geo_2_d I.geos_HausdorffDistance

nearestPoints :: Geometry -> Geometry -> Geos CoordinateSequence
nearestPoints g p = withGeos $ \h -> do
  ptr <-  withGeometry g $ \gp ->
            withGeometry p $ \pp ->
              I.geos_NearestPoints h gp pp
  fptr <- newForeignPtrEnv I.geos_CoordSeqDestroy h ptr
  return $ CoordinateSequence fptr

                          
{-area g = withGeos $ \h -> alloca $ \dptr -> do-}
    {-i <- throwIfZero (mkErrorMessage "area" ) $ withGeometry g $ \gp -> -}
        {-I.geos_Area h gp dptr -}
    {-s <- peek dptr-}
    {-return $ realToFrac s-}
