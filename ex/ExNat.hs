module ExNat where

import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise )

data Nat = Zero | Succ Nat

instance Show Nat where
    show Zero = "O"
    show (Succ x) = 'S' : show x

instance Eq Nat where
    (==) Zero Zero = True
    (==) (Succ u) (Succ v) = u == v
    (==) _ _ = False

instance Ord Nat where
    (<=) Zero _ = True
    (<=) _ Zero = False
    (<=) (Succ u) (Succ v) =  u <= v

    min (Succ u) (Succ v) = Succ (min u v)
    min _ _ = Zero

    max (Succ u) (Succ v) = Succ (max u v)
    max _ v = v

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero = True
even (Succ x) = not (even x)

odd :: Nat -> Bool
odd x = not (even x)

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) v Zero = v
(<+>) u (Succ v) = Succ (u <+> v)

-- This is called the dotminus or monus operator
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero _ = Zero
(<->) v Zero = v
(<->) u (Succ v) = pred (u <-> v)

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) v Zero = Zero
(<*>) u (Succ v) = u <+> (u <*> v)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ Zero = Succ Zero
(<^>) Zero _ = Zero
(<^>) u (Succ v) = u <*> (u <^> v)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) u v 
    | u < v = Zero
    | otherwise = Succ ((u <-> v) </> v)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) u v = u <-> ((u </> v) <*> v)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) u v = (u <%> v) == Zero

divides = (<|>)

absDiff :: Nat -> Nat -> Nat
absDiff u v
    | v <= u = u <-> v
    | otherwise = v <-> u

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero = Succ Zero
factorial (Succ x) = (Succ x) <*> (factorial x)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _ = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo u v
    | u < v = Zero
    | otherwise = Succ (lo (u </> v) v)

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat x = Succ (toNat (x - 1))

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat (Succ x) = 1 + (fromNat x)

instance Num Nat where
    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = error "Unable to convert negative number to Nat"
        | x == 0    = Zero
        | otherwise = Succ (fromInteger (x - 1))
        