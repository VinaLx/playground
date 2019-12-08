{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ExtensibleData where

newtype Expr f = In (f (Expr f))

data Val f = Val Int
data Add f = Add f f

data (f :+: g) e = Inl (f e) | Inr (g e)

instance Functor Val where
  fmap _ (Val x) = Val x

instance Functor Add where
  fmap f (Add a1 a2) = Add (f a1) (f a2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl fa) = Inl $ fmap f fa
  fmap f (Inr ga) = Inr $ fmap f ga

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In e) = f (fmap (foldExpr f) e)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject $ Val x

(<+>) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x <+> y = inject $ Add x y

infixl 6 <+>