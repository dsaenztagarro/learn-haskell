{-# LANGUAGE KindSignatures #-}
module Base.Monad.ExceptT where

import Control.Applicative
import Data.Kind (Type)
import Base.Monad.Identity
import Base.Monad.MonadTrans

newtype ExceptT (e :: Type) (m :: Type -> Type) (a :: Type) = ExceptT
  { runExceptT :: m (Either e a) }

type Except e = ExceptT e Identity

runExcept :: Except e a -> Either e a
runExcept = runIdentity . runExceptT

instance Functor m => Functor (ExceptT e m) where
  fmap f a = ExceptT $ (fmap . fmap) f (runExceptT a)

instance Monad m => Applicative (ExceptT e m) where
  pure a = ExceptT $ (pure . pure) a
  f <*> a = ExceptT $ do
    f' <- runExceptT f
    a' <- runExceptT a
    pure $ f' <*> a'

instance Monad m => Monad (ExceptT e m) where
  return = pure
  a >>= f = ExceptT $ do
    val <- runExceptT a
    case val of
      Left err -> pure $ Left err
      Right val' -> runExceptT $ f val'

instance (Monoid e, Monad m) => Alternative (ExceptT e m) where
  empty = ExceptT (pure $ Left mempty)
  a <|> b = ExceptT $ do
    a' <- runExceptT a
    case a' of
      Right val -> pure (Right val)
      Left err -> do
        b' <- runExceptT b
        case b' of
          Right val -> pure (Right val)
          Left err' -> pure (Left $ err <> err')

instance MonadTrans (ExceptT e) where
  lift m = ExceptT $ Right <$> m

throwError :: Monad m => e -> ExceptT e m a
throwError exception = ExceptT $ pure (Left exception)

catchError :: Monad m => (e -> ExceptT e m a) -> ExceptT e m a -> ExceptT e m a
catchError handler action = ExceptT $ do
  result <- runExceptT action
  case result of
    Left err -> runExceptT (handler err)
    Right val -> pure (Right val)

succeed :: Monad m => m a -> ExceptT e m a
succeed a = ExceptT (Right <$> a)
