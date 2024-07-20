{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Base.Monad.MonadState3Spec where

import Test.Hspec
import Base.Monad.StateT (StateT)
import qualified Base.Monad.StateT as StateT
import Base.Monad.ExceptT
import Base.Monad.MonadState

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


spec :: Spec
spec = do
  it "works using MonadState" $ do
    result <- evalGame "Foo" (guessTheState "Bar")
    result `shouldBe` Right False
    result' <- evalGame "Foo" (guessTheState "Foo")
    result' `shouldBe` Right True
