module Data.Geometry.Geos.STRTreeSpec where

import Data.Either
import qualified Data.Vector as V
import qualified Data.Geometry.Geos.STRTree as STR
import Data.Geometry.Geos.Geometry
import           Test.Hspec

strSpec = describe "STRTree" $ do
  describe "fromList" $ do
    it "should correctly build an STRTree" $ do
      let points = fmap (flip PointGeometry Nothing . point . uncurry coordinate2)
            [ (0.1, 0.2)
            , (0.2, 0.3)
            , (0.3, 0.4)
            ]
          mapping :: [(Geometry Point, Int)]
          mapping = zip points [1..]
          strtree = STR.fromList mapping
          env = V.fromList $ fmap (uncurry coordinate2)
            [ (0, 0)
            , (1, 0)
            , (1, 1)
            , (0, 1)
            , (0, 0)
            ]
          eitherQuery = linearRing  env >>=  polygon . V.singleton
          query = PolygonGeometry (fromRight (error "shouldn't happen") eitherQuery) Nothing
          results = STR.lookup query strtree
      results `shouldBe` (V.fromList [1..3])

  describe "unfoldr" $ do
    it "should correctly build an STRTree" $ do

      let buildFunc i =
            if i < 1
               then Just ((flip PointGeometry Nothing . point $ coordinate2 i i, 100 + i), i+0.1 )
               else Nothing
          init :: Double
          init = 0.1
          strtree = STR.unfoldr buildFunc init
          env = V.fromList $ fmap (uncurry coordinate2)
            [ (0, 0)
            , (1, 0)
            , (1, 1)
            , (0, 1)
            , (0, 0)
            ]
          eitherQuery = linearRing  env >>=  polygon . V.singleton
          query = PolygonGeometry (fromRight (error "shouldn't happen") eitherQuery) Nothing
          results = STR.lookup query strtree
      V.length results `shouldBe` 10
