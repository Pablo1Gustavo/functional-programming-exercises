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
    show (Succ n) = 'S' : show n

instance Eq Nat where
    (==) Zero Zero = True
    (==) (Succ n) (Succ m) = n == m
    (==) _ _ = False

instance Ord Nat where
    (<=) Zero _ = True
    (<=) _ Zero = False
    (<=) (Succ n) (Succ m) =  n <= m

    min (Succ n) (Succ m) = Succ (min n m)
    min _ _ = Zero

    max (Succ n) (Succ m) = Succ (max n m)
    max _ m = m

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

even :: Nat -> Bool
even Zero = True
even (Succ n) = not (even n)

odd :: Nat -> Bool
odd n = not (even n)

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) m Zero = m
(<+>) n (Succ m) = Succ (n <+> m)

-- This is called the dotminus or monus operator
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) m Zero = m
(<->) n (Succ m) = pred (n <-> m)

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) m Zero = Zero
(<*>) n (Succ m) = n <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ Zero = Succ Zero
(<^>) n (Succ m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) n m 
    | n < m = Zero
    | otherwise = Succ ((n <-> m) </> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) n m = n <-> ((n </> m) <*> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) n m = (n <%> m) == Zero

divides = (<|>)

absDiff :: Nat -> Nat -> Nat
absDiff n m
    | m <= n = n <-> m
    | otherwise = m <-> n

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero = Succ Zero
factorial (Succ n) = (Succ n) <*> (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _ = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo n m
    | n < m = Zero
    | otherwise = Succ (lo (n </> m) m)

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat (Succ n) = 1 + (fromNat n)

instance Num Nat where
    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger n
        | n < 0     = error "Unable to convert negative number to Nat"
        | n == 0    = Zero
        | otherwise = Succ (fromInteger (n - 1))
        