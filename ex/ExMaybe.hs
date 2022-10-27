module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust Nothing = error "Cannot get value from Nothing"
fromJust (Just m) = m

catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just m) = m

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = map fromJust . filter isJust . map f

maybe :: b -> (a -> b) -> Maybe a -> b
maybe d _ Nothing = d
maybe _ f (Just x) = f x;

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just m) = [m]

maybeApply :: Maybe (a -> a) -> a -> a
maybeApply Nothing x = x
maybeApply (Just f) x = f x

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = zipWith maybeApply
