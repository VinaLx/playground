{-# LANGUAGE RankNTypes #-}

module ExistentialEncoding where

data CounterInterface a
  = CounterInterface
  { newCounter :: a
  , tickUp     :: a -> a
  , get        :: a -> Int }

counterModule :: CounterInterface Int
counterModule = CounterInterface 0 (+1) id

unpack :: p x -> (forall a. p a -> r) -> r
unpack p cont = cont p

unpackCounterModule :: (forall a. CounterInterface a -> r) -> r
unpackCounterModule = unpack counterModule

tickUpTwice :: Int
tickUpTwice = unpackCounterModule $ \i ->
    get i $ tickUp i $ tickUp i $ newCounter i