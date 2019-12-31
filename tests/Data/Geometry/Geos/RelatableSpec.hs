module Data.Geometry.Geos.RelatableSpec
  ( 
    relatableSpec
  )
where

import           Test.Hspec
import           Helpers
import           Data.Geometry.Geos.Relatable

poly1 = fromRight $ makePolygonGeo [[(0, 0), (0, 1), (1, 1), (1.5, 1.5), (1, 0), (0, 0)]]

point1 = makePointGeo (0.5, 0.5)
point2 = makePointGeo (0.5, 1000.4)
point3 = makePointGeo (0, 0.5)


poly2 = fromRight $ makePolygonGeo
  [ [(0, 0), (0, 1), (1, 1), (1.5, 1.5), (1, 0), (0, 0)]
  , [(0.1, 0.1), (0.1, 0.9), (0.9, 0.1), (0.1, 0.1)]
  ]

poly3 = fromRight $ makePolygonGeo 
  [[(38, 40), (39, 40), (40, 42), (40, 44), (38, 44), (38, 40)]]

relatableSpec = describe "Relatable" $ do
  describe "contains" $ do
    it "should relate a polygon containing a point " $ contains poly1 point1 `shouldBe` True
    it "should relate a polygon not containing a point " $ contains poly1 point2 `shouldBe` False
  describe "touches" $ do
    it "should relate a polygon touching a point" $ touches poly1 point3 `shouldBe` True
    it "should relate a polygon not touching a point" $ touches poly1 point1 `shouldBe` False
  describe "intersects" $ do
    it "should relate a polyon intersecting another polygon" $ intersects poly1 poly2 `shouldBe` True
  describe "disjoint" $ do
    it "should relate disjoint polygons" $ disjoint poly1 poly3 `shouldBe` True
    it "should relate NON-disjoint polygons" $ disjoint poly1 poly2 `shouldBe` False

