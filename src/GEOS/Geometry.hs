module GEOS.Geometry (
  convertGeometryFromRaw,
  convertGeometryToRaw,
  interpolate,
  interpolateNormalized,
  project,
  projectNormalized

) where

import GEOS.Types
import qualified Data.Vector as V
import qualified GEOS.Raw.Geometry as R
import qualified GEOS.Raw.CoordSeq as RC
import GEOS.Raw.Base
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import GHC.Conc.Sync (pseq)


project :: Geometry -> Geometry -> Double
project g1 g2 = 
  let h = initializeGEOS putStrLn error
      g1'= convertGeometryToRaw h g1
      g2'= convertGeometryToRaw h g2
  in R.project h g1' g2'  

projectNormalized :: Geometry ->  Geometry -> Double
projectNormalized g1 g2 = 
  let h = initializeGEOS putStrLn error
      g1'= convertGeometryToRaw h g1
      g2'= convertGeometryToRaw h g2
  in R.project h g1' g2'  

interpolate :: Geometry -> Double -> Geometry 
interpolate g d = 
  let h = initializeGEOS putStrLn error
      g' = convertGeometryToRaw h g
  in convertGeometryFromRaw h $ R.interpolate h g' $  realToFrac d


interpolateNormalized :: Geometry -> Double -> Geometry 
interpolateNormalized g d = 
  let h = initializeGEOS putStrLn error
      g' = convertGeometryToRaw h g
  in convertGeometryFromRaw h $ R.interpolateNormalized h g' $  realToFrac d

binaryPredicate_ :: (GEOSHandle -> R.Geometry -> R.Geometry -> Bool)
                    -> Geometry 
                    -> Geometry 
                    -> Bool
binaryPredicate_ f g1 g2 = 
  let h = initializeGEOS putStrLn error
      g1' = convertGeometryToRaw h g1
      g2' = convertGeometryToRaw h g2
  in f h g1' g2'

disjoint :: Geometry -> Geometry -> Bool
disjoint = binaryPredicate_ R.disjoint

touches :: Geometry -> Geometry -> Bool
touches = binaryPredicate_ R.touches

crosses :: Geometry -> Geometry -> Bool
crosses = binaryPredicate_ R.crosses

within :: Geometry -> Geometry -> Bool
within = binaryPredicate_ R.within

contains :: Geometry -> Geometry -> Bool
contains = binaryPredicate_ R.contains

overlaps :: Geometry -> Geometry -> Bool
overlaps = binaryPredicate_ R.overlaps

equals :: Geometry -> Geometry -> Bool
equals = binaryPredicate_ R.equals

equalsExact :: Geometry -> Geometry -> Bool
equalsExact = binaryPredicate_ R.equalsExact

covers :: Geometry -> Geometry -> Bool
covers = binaryPredicate_ R.covers

coveredBy :: Geometry -> Geometry -> Bool
coveredBy = binaryPredicate_ R.coveredBy



convertGeometryToRaw :: GEOSHandle -> Geometry-> R.Geometry
convertGeometryToRaw h g = case g of
    PointGeometry pg s -> convertPointToRaw h pg s 
    LineStringGeometry lsg s -> convertLineStringToRaw h lsg s
    PolygonGeometry pg s -> convertPolygonToRaw h pg s 
    MultiPointGeometry mp s -> error "multipoint"
    MultiLineStringGeometry mls s -> error "multilineString"
    MultiPolygonGeometry mps s -> error "multipolygon"


convertPointToRaw :: GEOSHandle -> Point -> SRID -> R.Geometry
convertPointToRaw h p@(Point c) s = 
  let cs = RC.createCoordinateSequence h 1 (dimensions p)
      geo = setCoordinateSequence h cs 1 c `pseq` R.createPoint h cs
  in  R.setSRID h geo s `pseq` geo

convertLinearRingToRaw :: GEOSHandle -> LinearRing -> SRID -> R.Geometry
convertLinearRingToRaw h l@(LinearRing cs) s =
  let csr = RC.createCoordinateSequence h (V.length cs) (dimensions l) 
  in V.imap (setCoordinateSequence h csr) cs `pseq` R.createLinearRing h csr
  
convertLineStringToRaw :: GEOSHandle -> LineString -> SRID -> R.Geometry
convertLineStringToRaw h l@(LineString cs) s =
  let csr = RC.createCoordinateSequence h (V.length cs) (dimensions l) 
  in V.imap (setCoordinateSequence h csr) cs `pseq` R.createLineString h csr


convertPolygonToRaw ::GEOSHandle -> Polygon -> SRID -> R.Geometry
convertPolygonToRaw h p@(Polygon lrs) s = 
  let ext = convertLinearRingToRaw h (V.head lrs) s
      inn = V.toList $ fmap (\v -> convertLinearRingToRaw h v s) $ V.tail lrs
  in R.createPolygon h ext inn (length inn - 1) 


setCoordinateSequence :: GEOSHandle -> RC.CoordinateSequence -> Int -> Coordinate -> () 
setCoordinateSequence h cs i (Coordinate2 x y) = 
  RC.setCoordinateSequenceX h cs i x `pseq` RC.setCoordinateSequenceY h cs i y `pseq` ()
setCoordinateSequence h cs  i (Coordinate3 x y z) = 
  RC.setCoordinateSequenceX h cs i x `pseq` RC.setCoordinateSequenceY h cs i y `pseq` RC.setCoordinateSequenceZ h cs i z `pseq` ()

--- Conversions
--
convertGeometryFromRaw :: GEOSHandle -> R.Geometry -> Geometry
convertGeometryFromRaw h rg = let s = R.getSRID h rg
    in case R.getTypeId h rg of
          0 -> PointGeometry (convertPointFromRaw h rg ) s
          1 -> LineStringGeometry (convertLineStringFromRaw h rg) $! s 
          2 -> PolygonGeometry (convertPolygonFromRaw h rg) s
          3 -> MultiPointGeometry (convertMultiPointFromRaw h rg) s 
          4 -> MultiLineStringGeometry (convertMultiLineStringFromRaw h rg) s
          5 -> MultiPolygonGeometry (convertMultiPolygonFromRaw h rg) s
          e -> error $ "Unrecognized geometry type" <> show e 

getPosition :: GEOSHandle -> RC.CoordinateSequence -> Int -> Coordinate 
getPosition h cs i = 
    let dim = RC.getCoordinateSequenceDimensions h cs 
        x = RC.getCoordinateSequenceX h cs i   
        y = RC.getCoordinateSequenceY h cs i
        z = if dim == 3 
              then Just $ RC.getCoordinateSequenceZ h cs i
              else Nothing
    in case z of
      Nothing -> Coordinate2 x y
      Just z -> Coordinate3 x y z

convertPointFromRaw :: GEOSHandle -> R.Geometry -> Point
convertPointFromRaw h g = 
  let cs = R.getCoordinateSequence h g
  in Point $ getPosition h cs 0 

convertSequenceFromRaw :: GEOSHandle -> R.Geometry -> CoordinateSequence
convertSequenceFromRaw h g = 
  -- todo, consider using getCoordinateSequence_
  let cs = R.getCoordinateSequence h g
      size = R.getNumCoordinates h g
  in V.generate size (getPosition h cs)

convertLineStringFromRaw :: GEOSHandle -> R.Geometry -> LineString
convertLineStringFromRaw h g = LineString $ convertSequenceFromRaw h g

convertLinearRingFromRaw :: GEOSHandle -> R.Geometry -> LinearRing
convertLinearRingFromRaw h g = LinearRing $ convertSequenceFromRaw h g

convertPolygonFromRaw :: GEOSHandle -> R.Geometry -> Polygon
convertPolygonFromRaw h g = 
  let is = R.getNumInteriorRings h g
      ext = V.singleton $ R.getExteriorRing h g
      ins = V.generate is (R.getInteriorRingN h g)
  in Polygon $ (convertLinearRingFromRaw h) <$> (ext <> ins) 

convertMultiPointFromRaw :: GEOSHandle -> R.Geometry -> MultiPoint
convertMultiPointFromRaw h g =
  let ng = R.getNumGeometries h g
  in MultiPoint $ V.generate ng (\i -> convertPointFromRaw h $ R.getGeometryN h g i ) 
convertMultiLineStringFromRaw :: GEOSHandle -> R.Geometry -> MultiLineString
convertMultiLineStringFromRaw h g = 
  let ng = R.getNumGeometries h g
  in MultiLineString $ V.generate ng (\i -> convertLineStringFromRaw h $ R.getGeometryN h g i)

convertMultiPolygonFromRaw :: GEOSHandle -> R.Geometry -> MultiPolygon
convertMultiPolygonFromRaw h g = 
  let ng = R.getNumGeometries h g
  in MultiPolygon $ V.generate ng (\i -> convertPolygonFromRaw h $ R.getGeometryN h g i)