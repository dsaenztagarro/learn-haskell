{-# LANGUAGE StandaloneDeriving #-}

module Std.Data.MaybeSpec (spec) where

import Prelude hiding ((>>=))
import Test.Hspec
import qualified Std.Data.Maybe as M
import Std.Data.Maybe (bound, half)
import Std.Control.Monad ((>>=))

-- Std.Data.Maybe deliberately omits Eq/Show; derive them here for assertions.
deriving instance Eq a   => Eq   (M.Maybe a)
deriving instance Show a => Show (M.Maybe a)

spec :: Spec
spec = do
  describe "half" $ do
    it "passes through an even number" $
      half 10 `shouldBe` M.Just 10
    it "rejects an odd number" $
      half 11 `shouldBe` M.Nothing

  describe "bound" $ do
    it "passes a value within range" $
      bound (0, 20) 11 `shouldBe` M.Just 11
    it "rejects below minimum" $
      bound (0, 20) (-1) `shouldBe` M.Nothing
    it "rejects above maximum" $
      bound (0, 20) 21 `shouldBe` M.Nothing

  describe "monadic chaining (Effective Haskell example)" $ do
    it "bound (0,20) 11, then succ, then half == Just 12" $
      (bound (0, 20) 11 >>= \x -> M.Just (succ x) >>= half)
        `shouldBe` M.Just 12
    it "short-circuits on the first Nothing" $
      (bound (0, 20) 21 >>= \x -> M.Just (succ x) >>= half)
        `shouldBe` M.Nothing
