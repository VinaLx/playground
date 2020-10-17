{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module ContT where

import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }


instance Functor (ContT r m) where
  -- Continuation that produces `a` to continuation that produces `b`
  fmap f (ContT c) = ContT $ \k -> c (k . f)

instance Applicative (ContT r m) where
  -- Continuation that produce a specific value
  pure a = ContT ($ a)
  -- Wait for `cf` to produce `f`, then wait for `ca` to produce `a`, then we
  -- have (f a) for the callback
  ContT cf <*> ContT ca = ContT $ \kb -> cf $ \f -> ca $ \a -> kb (f a)

instance Monad (ContT r m) where
  return = pure
  -- Wait for `cf` to produce `a`, then we can produce a new procedure
  -- that produce `b`, then the external requirement is satisfied.
  ContT c >>= f = ContT $ \k -> c $ \a -> runContT (f a) $ \b -> k b

-- runContT (mapContT f c) = f . runContT c
mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f c = ContT $ f . runContT c

-- runContT (withContT f c) = runContT c . f
withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f c = ContT $ runContT c . f

class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (ContT r m) where
  -- callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
  callCC f = ContT $ \ka ->
    runContT (f $ \a -> ContT $ \_kb -> ka a) $ \a -> ka a

type Cont r a = ContT r Identity a

runCont :: Cont r a -> (a -> r) -> r
runCont c f = runIdentity $ runContT c (Identity . f)

cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT $ \k -> Identity $ f (runIdentity . k)

instance MonadCont m => MonadCont (StateT s m) where
  -- callCC :: ((a -> StateT s m b) -> StateT s m a) -> StateT s m a
  callCC f = StateT $ \s ->
    callCC $ \ret ->
      (`runStateT` s) $ f $ \a ->
        StateT $ \_ -> ret (a, s)

instance MonadCont m => MonadCont (ReaderT s m) where
  callCC f = ReaderT $ \s ->
    callCC $ \ret ->
      runReaderT (f $ ReaderT . const . ret) s

instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
  callCC f = WriterT $ callCC $ \ret ->
    runWriterT $ f $ \a -> WriterT (ret (a, mempty))

instance MonadCont m => MonadCont (ExceptT e m) where
  callCC f = ExceptT $ callCC $ \ret ->
    runExceptT $ f $ ExceptT . ret . Right

instance MonadTrans (ContT r) where
  lift = ContT . (>>=)

instance MonadState s m => MonadState s (ContT r m) where
  get = lift get
  put = lift . put

firstNegative'
  :: [Int]
  -> (Int -> StateT Int (ContT r Identity) b)
  -> StateT Int (ContT r Identity) Int
firstNegative' [] _ = return 0
firstNegative' (x : xs) ret = modify (+1) >> if x >= 0
  then firstNegative' xs ret
  else ret x >> undefined

firstNegative :: [Int] -> StateT Int (ContT r Identity) Int
firstNegative xs = callCC (firstNegative' xs)

run:: [Int] -> (Int, Int)
run = (`runCont` id) . (`runStateT` 0) . firstNegative

firstNegative2 :: [Int] -> ContT r (State Int) Int
firstNegative2 xs = callCC (firstNegative2' xs)
  where
    firstNegative2'
      :: [Int]
      -> (Int -> ContT r (State Int) b)
      -> ContT r (State Int) Int
    firstNegative2' [] _ = return 0
    firstNegative2' (x : xs) ret = modify (+1) >> if x >= 0
      then firstNegative2' xs ret
      else ret x >> undefined

run2 :: [Int] -> (Int, Int)
run2 = (`runState` 0) . (`runContT` return) . firstNegative2
