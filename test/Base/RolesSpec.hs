module Base.RolesSpec (spec) where

import Test.Hspec
import Base.Roles

spec :: Spec
spec = do
  describe "Set Num instance" $ do
    it "adds element-wise" $
      MkSet (3 :: Int) + MkSet 4 `shouldBe` MkSet 7
    it "multiplies element-wise" $
      MkSet (3 :: Int) * MkSet 4 `shouldBe` MkSet 12

  describe "MySet (deriving Num via Set Int)" $ do
    -- This block exists primarily to ensure the role annotation
    -- on Set permits the `deriving Num via Set Int` line. If roles
    -- were nominal, the test suite would not compile.
    it "derives + via Set Int" $
      let s1 = MySet (MkSet (NotInt 3))
          s2 = MySet (MkSet (NotInt 4))
      in getSet (s1 + s2) `shouldBe` MkSet (NotInt 7)

  describe "BadIdea" $ do
    it "Int instance returns the original predicate" $
      bad (5 :: Int) `shouldBe` True
    it "Int instance rejects non-positive" $
      bad (0 :: Int) `shouldBe` False
