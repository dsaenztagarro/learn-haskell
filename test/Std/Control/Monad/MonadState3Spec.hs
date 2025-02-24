{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Std.Control.Monad.MonadState3Spec where

import Test.Hspec
import Std.Control.Monad.StateT (StateT)
import qualified Std.Control.Monad.StateT as StateT
import Std.Control.Monad.ExceptT
import Std.Control.Monad.MonadState

{-
Since basic operations like `get` and `put` depend on the `MonadState` type
class rather than a particular type, we can refactor our program to make
`GuessingGame` newtype without having to change `guessTheState` at all.
-}

newtype GuessingGame a =
  GuessingGame { runGame :: ExceptT String (StateT String IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadState String)

evalGame :: String -> GuessingGame a -> IO (Either String a)
evalGame input =
  flip StateT.evalStateT input . runExceptT . runGame

guessTheState :: String -> GuessingGame Bool
guessTheState guess = do
  answer <- get
  put guess
  pure $ guess == answer

guessTheLength :: Int -> GuessingGame Bool
guessTheLength guess = do
  answer <- length <$> get
  pure $ guess == answer


spec :: Spec
spec = do
  it "works using MonadState" $ do
    result <- evalGame "Foo" (guessTheState "Bar")
    result `shouldBe` Right False
    result' <- evalGame "Foo" (guessTheState "Foo")
    result' `shouldBe` Right True

  it "works using MonadState thanks to FunctionalDependencies" $ do
    result <- evalGame "Foo" (guessTheLength 4)
    result `shouldBe` Right False
    result' <- evalGame "Foo" (guessTheLength 3)
    result' `shouldBe` Right True
