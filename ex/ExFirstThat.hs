module ExFirstThat where

import Prelude hiding ( maybe, Maybe(..) )
import ExMaybe

-- Q: Who is the first of these that has the property p?
-- Possible answers
-- * Nobody
-- * Just that guy
firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat p = listToMaybe . filter p

isMaybeGood :: (a -> Bool) -> Maybe a -> Maybe Bool
isMaybeGood p Nothing = Nothing
isMaybeGood p (Just x) = Just (p x)

-- Q: What do you have to say about the first of these that has property p?  Do you like them or not?
-- Possible answers:
-- * Just a simple (yes/no)
-- * Nothing to say (cause nobody has this property)
isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat p q = fmap p . firstThat q