module Base.Monad.MonadState2Spec where

import Test.Hspec
import Base.Monad.StateT (StateT)
import qualified Base.Monad.StateT as StateT
import Base.Monad.ExceptT
import Base.Monad.MonadState

type GuessingGame a = ExceptT String (StateT String IO) a

evalGame :: String -> GuessingGame a -> IO (Either String a)
evalGame input =
  flip StateT.evalStateT input . runExceptT

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
