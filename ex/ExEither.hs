module ExEither where

import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromLeft :: a -> Either a b -> a
fromLeft d (Right _) = d
fromLeft _ (Left x) = x

fromRight :: b -> Either a b -> b
fromRight d (Left _) = d
fromRight _ (Right x) = x

lefts :: [Either a b] -> [a]
lefts = map left' . filter isLeft
    where
        left' (Left x) = x

rights :: [Either a b] -> [b]
rights = map right' . filter isRight
    where
        right' (Right x) = x

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers es = (lefts es, rights es)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right x) = g x

