{-# LANGUAGE KindSignatures #-}

module TypeFixPoint where

newtype Fix (f :: * -> *) = Wrap { unwrap :: f (Fix f) }

data ListF a l = Nil | Cons a l

type List a = Fix (ListF a)

nil :: List a
nil = Wrap Nil

cons :: a -> List a -> List a
cons a l = Wrap $ Cons a l

foldrF :: (a -> b -> b) -> b -> List a -> b
foldrF step z l = case unwrap l of
    Nil -> z
    Cons a l2 -> step a (foldrF step z l2)
