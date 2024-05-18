module NatConversions(intToNat, natToInt) where

import Nat

intToNat :: Int -> Nat
intToNat x | x <= 0 = Zero
intToNat x = Succ (intToNat (x - 1))


natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ x) = 1 + natToInt x