{-# LANGUAGE RankNTypes #-}

module FreeMonad where

import Control.Monad

data Free f a
  = Pure a
  | Roll (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Roll m) = Roll $ fmap (fmap f) m

instance Functor f => Applicative (Free f) where
  pure = Pure

  mf <*> ma = mf >>= (<$> ma)

instance Functor f => Monad (Free f) where
  return = Pure

  Pure a >>= f = f a
  Roll m >>= f = Roll $ fmap (>>= f) m

interpret :: (Functor f, Monad m) => (forall a. f a -> m a) -> Free f a -> m a
interpret _ (Pure a) = return a
interpret f (Roll m) = join $ f (fmap (interpret f) m)