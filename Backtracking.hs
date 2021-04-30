{-# LANGUAGE LambdaCase #-}

module Backtracking where

import Control.Monad.Except

data BT e a
  = Zero e
  | One  a
  | Plus (BT e a) (e -> BT e a)


runBT :: BT e a -> Either e a
runBT = \case
  Zero e -> throwError e
  One a -> return a
  Plus bt k -> catchError (runBT bt) (fmap runBT k)

instance Functor (BT e) where
  fmap f = \case
    Zero e -> Zero e
    One  a -> One (f a)
    Plus bt k -> Plus (fmap f bt) (fmap (fmap f) k)

instance Applicative (BT e) where
  pure a = One a
  mf <*> ma = case mf of
    Zero e -> Zero e
    One f -> f <$> ma
    Plus mf' k -> Plus (mf' <*> ma) (fmap (<*> ma) k)

instance Monad (BT e) where
  return a = One a
  m >>= f = case m of
    Zero e -> Zero e
    One  a -> f a
    Plus m k -> Plus (m >>= f) (fmap (>>= f) k)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

search :: (a -> Bool) -> Tree a -> BT () a
search p Leaf = Zero ()
search p (Branch l a r) | p a = One a
search p (Branch l a r) = Plus (search p l) (const $ search p r)

search' :: (a -> Bool) -> Tree a -> BT () a
search' p Leaf = Zero ()
search' p (Branch l a r) =
  Plus test $ const $ Plus (search' p l) $ const (search' p r)
  where test = if p a then One a else Zero ()

btLt :: Int -> Int -> BT () ()
btLt m n = if m < n then One () else Zero ()

t :: Tree Int
t = Branch
  (Branch (Branch Leaf 1 Leaf) 3 (Branch Leaf 2 Leaf)) 4
  (Branch (Branch Leaf 5 Leaf) 6 (Branch Leaf 7 Leaf))

-- Left ()
example1 :: Either () ()
example1 = runBT $ do
  a <- search even t
  a `btLt` 3

-- Right ()
example2 :: Either () ()
example2 = runBT $ do
  a <- search' even t
  a `btLt` 3