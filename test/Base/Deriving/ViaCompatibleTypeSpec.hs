module Base.Deriving.ViaCompatibleTypeSpec (spec) where

import Test.Hspec
import Base.Deriving.ViaCompatibleType

test_semigroup :: MyMaybe [Int] -> MyMaybe [Int] -> MyMaybe [Int]
test_semigroup m1 m2 = m1 <> m2

spec :: Spec
spec = do
  it "derives type class instance" $ do
    -- Semigroup
    let r1 = test_semigroup (MyMaybe $ Just [1,2,3]) (MyMaybe $ Just [3,4,5])
    r1 `shouldBe` MyMaybe (Just [1,2,3])
    let r2 = test_semigroup (MyMaybe Nothing) (MyMaybe $ Just [3,4,5])
    r2 `shouldBe` MyMaybe (Just [3,4,5])


