{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module FreeMonad where

import Control.Monad

data Free f a
  = Pure a
  | Roll (f (Free f a))

data F a = F a a

type BinaryTree a = Free F a

data Free' :: (* -> *) -> * -> * where
  Pure'    :: a   -> Free' f a
  Suspend' :: f a -> Free' f a
  Bind'    :: Free' f a -> (a -> Free' f b) -> Free' f b

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

interpret' :: Functor f => (forall a. f a -> a) -> Free f a -> a
interpret' _ (Pure a) = a
interpret' f (Roll m) = f (fmap (interpret' f) m)

interpret :: (Functor f, Monad m) => (forall a. f a -> m a) -> Free f a -> m a
interpret _ (Pure a) = return a
interpret f (Roll m) = join $ f (fmap (interpret f) m)
