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
mapMaybe = undefined

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = undefined

maybeToList :: Maybe a -> [a]
maybeToList = undefined

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = undefined

