
module Data.Geometry.Geos.TopologySpec
  ( topologySpec
  )
where

import           Test.Hspec
import qualified Data.Vector                   as V
import           Data.Either hiding (fromRight)
import           Helpers
import           Data.Geometry.Geos.Geometry
import           Data.Geometry.Geos.Topology



poly1 = fromRight $ makePolygonGeo [[(0, 0), (0, 1), (1, 1), (1.5, 1.5), (1, 0), (0, 0)]]

cornerOnlyPoly = fromRight $ makePolygonGeo [[(1.0, 1.0), (1.5, 1.5), (1.0, 0.0), (1.0, 1.0)]]

poly2 = fromRight $ makePolygonGeo
  [ [(0, 0), (0, 1), (1, 1), (1.5, 1.5), (1, 0), (0, 0)]
  , [(0.1, 0.1), (0.1, 0.9), (0.9, 0.1), (0.1, 0.1)]
  ]
poly2Hull =
  fromRight $ makePolygonGeo [[(0.0, 0.0), (0.0, 1.0), (1.5, 1.5), (1.0, 0.0), (0.0, 0.0)]]
poly3 =
  fromRight $ makePolygonGeo [[(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (0.0, 0.0)]]
poly3Boundary = fromRight $ makeLineStringGeo [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (0.0, 0.0)]
poly4 =
  fromRight $ makePolygonGeo [
    [(1.0, 1.0), (1.0, 2.0), (2.0, 2.0), (2.0, 1.0), (1.0, 1.0)]
  ]
poly5 = 
  fromRight $ makePolygonGeo [[(0.0, 0.0),(0.0, 1.0),(1.0, 1.0),(1.0, 2.0),(2.0, 2.0),(2.0, 1.0),(1.3333333333333333, 1.0),(1.0, 0.0),(0.0, 0.0)]]
env1 =
  fromRight $ makePolygonGeo [[(0.0, 0.0), (1.5, 0.0), (1.5, 1.5), (0.0, 1.5), (0.0, 0.0)]]
lineString1 = fromRight $ makeLineStringGeo [(0, 0), (0, 1), (1, 1)]
lineString1Hull = fromRight $ makePolygonGeo [[(0, 0), (0, 1), (1, 1), (0, 0)]]
point1 = makePointGeo (0.5, 1.0)

topologySpec = describe "Topology" $ do
  describe "envelope" $ do
    it "should create an envelope for a plain polygon" $ (envelope poly1 >>= ensurePolygon)  `shouldBe` Just env1
    it "should create an envelope for a polygon with holes" $ (envelope poly2 >>= ensurePolygon)  `shouldBe` Just env1
    it "should create an envelope for a linestring" $ (envelope lineString1 >>= ensurePolygon) `shouldBe` Just poly3
  describe "intersection" $  do
    it "should create a point for polygons with a single-point intersection" $ (intersection poly3 poly4 >>= ensurePoint) `shouldBe` (Just $ makePointGeo (1.0, 1.0))
  describe "convexHull" $ do
    it "should make a convex hull from a polygon" $ convexHull poly2 `shouldBe` Just poly2Hull
    it "should make a convex hull from a linestring" $ convexHull lineString1 `shouldBe` Just lineString1Hull
    it "should not make a convex hull from a point" $ convexHull point1 `shouldBe` Nothing
  describe "difference" $ do
    it "should compute the difference between polygons" $ (difference poly1 poly3  >>= ensurePolygon) `shouldBe` Just cornerOnlyPoly
    it "should compute the difference between a polygon and a linestring" $ do
      (difference poly4 lineString1 >>= ensurePolygon) `shouldBe` Just poly4
  describe "boundary" $ do
    it "should compute the boundary of a polygon" $ do
      (boundary poly3 >>= ensureLineString) `shouldBe` Just poly3Boundary
  describe "union" $ do 
      it "should compute the union of two polygons" $ do
        (union poly1 poly4) `shouldBe` (Just $ Some poly5)
  describe "unaryUnion"  $ do 
      it "should compute the unary union of a multi polygon" $ 
        let pgons = fmap (\case (PolygonGeometry p _)  -> p) [poly1, poly4]
            multiPgon = MultiPolygonGeometry (multiPolygon $ V.fromList pgons) Nothing
        in (unaryUnion multiPgon) `shouldBe` (Just $ Some poly5)
  describe "centroid"  $ do
    it "should compute the centroid of a polygon" $ 
      centroid poly1 `shouldBe` (Just $ PointGeometry (point $ coordinate2 0.6333333333333333 0.5666666666666667) Nothing)
  describe "delaunayTriangulation"  $  do
    it "should compute the delaunay triangulation of a simple polygon" $ do
      let ls1 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(0.0, 1.0),(1.5, 1.5) ] )
          ls2 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(0.0, 0.0),(0.0, 1.0) ] )
          ls3 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(0.0, 0.0),(1.0, 0.0) ] )
          ls4 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(1.0, 0.0),(1.5, 1.5) ] )
          ls5 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(1.0, 0.0),(1.0, 1.0) ] )
          ls6 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(1.0, 1.0),(1.5, 1.5) ] )
          ls7 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(0.0, 1.0),(1.0, 1.0) ] )
          ls8 = lineString $  V.map (uncurry coordinate2) ( V.fromList [(0.0, 1.0),(1.0, 0.0) ] )
          lineStrings = multiLineString $ V.fromList $ rights [
            ls1, ls2, ls3, ls4, ls5, ls6, ls7, ls8 ]

       in (delaunayTriangulation poly1 0.1 True >>= ensureMultiLineString) `shouldBe` (Just $ MultiLineStringGeometry lineStrings Nothing)
  describe "voronoiDiagram" $ do
    it "should compute the voronoi diagram of a polygon" $ 
      let p1 = makePolygonGeo [[(0.5, 0.5), (0.5, 2.0), (2.0, 0.5), (0.5, 0.5)]]
          p2 = makePolygonGeo [[(-1.5, 0.5), (-1.5, 3.0), (0.16666666666666652, 3.0), (0.5, 2.0), (0.5, 0.5), (-1.5, 0.5)]]
          p3 = makePolygonGeo [[(0.5, -1.5), (-1.5, -1.5), (-1.5, 0.5), (0.5, 0.5), (0.5, -1.5)]]
          p4 = makePolygonGeo [[(3.0, 0.16666666666666663), (3.0, -1.5), (0.5, -1.5), (0.5, 0.5), (2.0, 0.5), (3.0, 0.16666666666666663)]]
          p5 = makePolygonGeo [[(0.16666666666666652, 3.0), (3.0, 3.0), (3.0, 0.16666666666666663), (2.0, 0.5), (0.5, 2.0), (0.16666666666666652, 3.0)]]
          collection = geometryCollection $ V.fromList $ Some <$> rights [p1, p2,  p3, p4, p5 ]

       in ( ensureGeometryCollection $ voronoiDiagram poly1 Nothing 0.1 False) `shouldBe` (Just $ CollectionGeometry collection Nothing)

  describe "polygonize" $ do
    it "should polygonize a set of polygons" $ do
      let result = fromRight $ makePolygonGeo $ [[(0.1, 0.1), (0.1, 0.9), (0.9, 0.1), (0.1, 0.1)]]
      (polygonize $ V.fromList [poly1, poly2]) `shouldBe` Just result

  describe "minimumRotatedRectangle" $ do
    it "should create a minumum rotated rectangle from a polygon" $
      let poly = fromRight $ makePolygonGeo [[(-0.3, 0.9), (9.999999999999999e-2, -0.3), (1.9, 0.3), (1.5, 1.5), (-0.3, 0.9)]]
       in minimumRotatedRectangle poly1 `shouldBe` Just poly
  describe "minimumClearance" $ do 
    it "should compute the minimum clearance for a polygon" $ do
      minimumClearance poly1 `shouldBe` Just 0.316227766016838
