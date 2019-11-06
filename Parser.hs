{-# LANGUAGE RankNTypes #-}

module Parser where

import           Control.Applicative

newtype Parser a
  = P { unP :: forall r. String -> (a -> String -> r) -> r -> r }

instance Functor Parser where
    fmap f (P k) = P $ \s ar e -> k s (ar . f) e

instance Applicative Parser where
    pure a = P $ \s ar _ -> ar a s
    P kf <*> P ka = P $ \s br e ->
        kf s (\f s' -> ka s' (\a s'' -> br (f a) s'') e) e

instance Monad Parser where
    return = pure
    P ka >>= f = P $ \s br e ->
        ka s (\a s' -> unP (f a) s' br e) e

instance Alternative Parser where
    empty = P $ const (const id)
    P kl <|> P kr = P $ \s ar e -> kl s ar $ kr s ar e

runP :: Parser a -> String -> Maybe a
runP (P k) s = k s (\a _ -> Just a) Nothing

fail :: Parser a
fail = empty

success :: a -> Parser a
success = return

consume :: Parser Char
consume = P $ \s cr e -> case s of
    [] -> e
    (c : s') -> cr c s'

