import Test.QuickCheck

import Nat
import NatConversions


propNatInt :: Nat -> Bool
propNatInt x = intToNat (natToInt x) == x

propIntNat :: Int -> Bool
propIntNat x = natToInt (intToNat x) == x

-- instance Arbitrary Q where
    -- arbitrary = do 
                -- Positive m <- arbitrary // Positive n <- arbitrary // return Q(m, n)



instance Arbitrary Nat where
    arbitrary = do
        Positive m <- arbitrary
        return (intToNat m)


main :: IO ()
main = do
    quickCheck propNatInt
    quickCheck propIntNat