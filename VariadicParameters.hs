{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module VariadicParameters where

import Data.List

class VariadicPrint a r | a -> r where
    prints :: [String] -> ([String] -> r) -> a

instance VariadicPrint r r where
    prints ss f = f (reverse ss)

instance (Show e, VariadicPrint a r) => VariadicPrint (e -> a) r where
    prints ss f e = prints (show e : ss) f

printLines :: VariadicPrint a String => a
printLines = prints [] unlines

printLines1 :: (VariadicPrint a String, Show e) => e -> a
printLines1 = prints [] unlines

printWords :: VariadicPrint a String => a
printWords = prints [] unwords