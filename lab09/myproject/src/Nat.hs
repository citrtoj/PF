module Nat(add, Nat(..)) where

data Nat = Zero | Succ Nat deriving (Show, Eq)

addAux :: Nat -> Nat -> Nat -> Nat
addAux Zero Zero a = a
addAux Zero (Succ y) a = addAux Zero y (Succ a)
addAux (Succ x) y a = addAux x y (Succ a)

add :: Nat -> Nat -> Nat
add x y = addAux x y Zero

-- add :: Nat -> Nat -> Nat
-- add Zero y = y
-- add (Succ x) y = Succ (add x y)

