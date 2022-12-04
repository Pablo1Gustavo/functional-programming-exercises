module Funktor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
    fmap :: (a -> b) -> f a -> f b

    (<$) :: b -> f a -> f b
    (<$) = fmap . const

instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ _ = Nothing

instance Funktor (Either a) where
    fmap f (Left e) = Left e
    fmap f (Right x) = Right (f x)

instance Funktor ((->) a) where
    fmap = (.)

instance Funktor ((,) a) where
    fmap f (l,r) = (l, f r)
