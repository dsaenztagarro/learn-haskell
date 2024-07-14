module Base.Monad.ExceptTSpec where

import Test.Hspec
import Base.Monad.ExceptT
import Base.Monad.State
import Control.Monad

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------
-- Parsing with ExceptT
--------------------------

type Parser a = ExceptT String (State Text) a

spec :: Spec
spec = do
  it "todo" $ do
    1 `shouldBe` 1
