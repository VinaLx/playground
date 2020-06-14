{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DependentType where

-- Lifted type level `Nat` thanks to DataKinds
data Nat where
    Z :: Nat
    S :: Nat -> Nat

-- "Singleton" type for the type level nat
data SNat n where
    SZ :: SNat 'Z
    SS :: SNat n -> SNat ('S n)

-- type level computation
type family Plus (m :: Nat) (n :: Nat) where
    Plus 'Z     n = n
    -- Require "UndecidableInstances"
    Plus ('S m) n = Plus m ('S n)

data Vector a n where
    Nil  :: Vector a 'Z
    Cons :: a -> Vector a n -> Vector a ('S n)
