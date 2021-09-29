{-# LANGUAGE DeriveFunctor #-}

module NonTerm where

import Control.Applicative

-- | Invariant:
-- | forall n v. f n = Some v -> forall { n' | n' > n }. f n' = Some v
newtype NT a = NT { unNT :: Int -> Maybe a}
    deriving Functor

diverge :: NT a
diverge = NT $ const Nothing

instance Applicative NT where
    pure = NT . const . return
    NT f <*> NT a = NT $ \n -> liftA2 ($) (f n) (a n)

instance Monad NT where
    return = pure
    NT a >>= f = NT $ \n -> a n >>= ($ n). unNT . f

type Rec a b = (a -> NT b) -> a -> NT b

ntFix' :: Rec a b -> Int -> a -> NT b
ntFix' f 0 a = diverge
ntFix' f n a = f (ntFix' f (n - 1)) a

ntFix :: Rec a b -> a -> NT b
ntFix f a = NT $ \n -> unNT (ntFix' f n a) n

quickSort :: Ord a => [a] -> NT [a]
quickSort = ntFix quickSort'
  where
    quickSort' quickSort'' xs = case xs of
      []  -> return []
      [a] -> return [a]
      a : xs' -> do
        l <- quickSort'' (filter (<= a) xs')
        r <- quickSort'' (filter (>  a) xs')
        return $ l ++ [a] ++ r