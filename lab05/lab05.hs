-- --
-- 0.3

data Nat = Cons [Bool] -- lista de valori de tip boolean. O lista goala va fi considerata ca avand valoarea 0.

showNatUtil :: Nat -> String
showNatUtil (Cons []) = ""
showNatUtil (Cons (x:xs)) = (if x then "1" else "0") ++ showNatUtil(Cons xs)

instance Show Nat where
    show (Cons []) = "0b0"
    show (Cons list) = "0b" ++ showNatUtil (Cons list) -- lista are biti in ea

convertBinaryNatToInteger :: Nat -> Integer
convertBinaryNatToInteger (Cons list) = fst (foldr (\element (total, factor) -> (total + (if element then factor else 0), factor * 2)) (0, 1) list)

convertIntegerToBinaryNatAux :: Integer -> Nat -> Nat
convertIntegerToBinaryNatAux x natX | x == 0 = natX
convertIntegerToBinaryNatAux x (Cons list) = let (quot, rem) = quotRem x 2 in
                                             convertIntegerToBinaryNatAux quot (Cons ((rem == 1) : list))

convertIntegerToBinaryNat :: Integer -> Nat
convertIntegerToBinaryNat x | x <= 0 = Cons [] -- ca sa nu ma complic cu Maybe Nat sau cu reprezentari cu semn pt ca mna, Nat...
convertIntegerToBinaryNat x = convertIntegerToBinaryNatAux x (Cons [])


-- 0.4

instance Eq Nat where
    -- cazurile speciale unde 0 poate fi reprezentat in 2 moduri, pt ca lista poate fi goala
    (==) (Cons []) (Cons [False]) = True 
    (==) (Cons [False]) (Cons []) = True
    (==) z@(Cons x) t@(Cons y) = convertBinaryNatToInteger z == convertBinaryNatToInteger t

instance Ord Nat where
    (<=) z@(Cons x) t@(Cons y) = convertBinaryNatToInteger z <= convertBinaryNatToInteger t

instance Num Nat where
    fromInteger = convertIntegerToBinaryNat
    (+) x y = fromInteger (convertBinaryNatToInteger x + convertBinaryNatToInteger y)
    (*) x y = fromInteger (convertBinaryNatToInteger x * convertBinaryNatToInteger y)
    abs x = x -- ca s toate pozitive oricum.
    signum x | x == Cons [] = 0
    signum x = 1 -- all others are positive
    x - y | x < y = 0
    x - y = fromInteger (convertBinaryNatToInteger x - convertBinaryNatToInteger y)

boundToPositiveInt :: Integer -> Int
boundToPositiveInt x
    | x <= 0 = 0
    | x >= toInteger (maxBound :: Int) = maxBound
    | otherwise = fromInteger x

instance Enum Nat where
    fromEnum x = boundToPositiveInt $ convertBinaryNatToInteger x
    toEnum x | x <= 0 = Cons []
    toEnum x = convertIntegerToBinaryNat $ toInteger x

instance Real Nat where
    toRational x = toRational $ convertBinaryNatToInteger x

instance Integral Nat where
    quotRem x y = let (quot, rem) = quotRem (convertBinaryNatToInteger x) (convertBinaryNatToInteger y) in
                  (convertIntegerToBinaryNat quot, convertIntegerToBinaryNat rem)
    toInteger = convertBinaryNatToInteger


-- 0.5

data Complex a = Num a => Comp a a

instance Show a => Show (Complex a) where
    show (Comp a b) = show a ++ "+(" ++ show b ++ ")i" 

divComplex :: (Num a, Floating a) => Complex a -> Complex a -> Complex a
divComplex (Comp a b) (Comp c d) = let numitor = (c ** 2 + d ** 2) in
                                   Comp ((a * c + b * d) / numitor) ((b * c - a * d) / numitor)

instance (Num a, Floating a) => Num (Complex a) where -- am cerut ca si a sa fie floating ca un complex de tip abs sa poata exista
    (+) (Comp x y) (Comp z t) = Comp (x + z) (y + t)
    (-) :: (Num a, Floating a) => Complex a -> Complex a -> Complex a
    (-) (Comp x y) (Comp z t) = Comp (x - z) (y - t)
    (*) (Comp x y) (Comp z t) = Comp (x * z - y * t) (x * t + y * z)
    negate (Comp x y) = Comp (-x) (-y)
    abs (Comp x y) = Comp (sqrt (x ** 2 + y ** 2)) 0
    signum z@(Comp x y) = let absz = abs z in
                          divComplex z absz     -- signum of z is z / abs z
    fromInteger z = Comp (fromInteger z) 0


-- 0.6

data MyOrdering = MyLT | MyEQ | MyGT deriving Eq

class Eq a => MyOrd a where -- imi trb "Eq a" ca sa pot sa fac minimalu suficient de minimal.
    myCompare :: a -> a -> MyOrdering
    leq :: a -> a -> Bool

    -- Defaults:
    -- - myCompare
    myCompare x y | not $ leq x y = MyGT
    myCompare x y | x == y = MyEQ
    myCompare x y | leq x y = MyLT
    -- - leq
    leq x y = myCompare x y /= MyGT

    {-# MINIMAL myCompare | leq #-}


-- 0.6.1

instance MyOrd Int where
    leq x y = x <= y

-- ca sa testez mai usor functia sort
instance MyOrd Integer where
    leq x y = x <= y

-- 0.6.2

instance MyOrd a => MyOrd [a] where
    myCompare [] [] = MyEQ
    myCompare [] _  = MyLT
    myCompare _  [] = MyGT
    myCompare (hd1 : tl1) (hd2 : tl2) | hd1 == hd2 = myCompare tl1 tl2
                                      | otherwise = myCompare hd1 hd2


-- 0.6.3

sort :: MyOrd a => [a] -> [a]
sort [] = []
sort (hd : tl) = sort (filter (`leq` hd) tl) ++ [hd] ++ sort (filter (\el -> not (leq el hd)) tl)

myArray :: [Int]
myArray = [2, 5, 1, 4, 7]


-- 0.7

-- data Nat' = Zero | Succ Nat' deriving (Show, Eq, Ord)
-- le si ordoneaza in functie de constructori :) nice.

-- 0.8, 0.9

data Nat' = Zero | Succ Nat'

instance Show Nat' where
    show Zero = "o"
    show (Succ x) = "s" ++ show x

-- (include astfel si 0.12)
instance Eq Nat' where
    (==) Zero Zero = True
    (==) (Succ x) (Succ y) = x == y
    (==) _ _ = False

instance Ord Nat' where
    (<=) Zero _ = True
    (<=) _ Zero = False
    (<=) (Succ x) (Succ y) = x <= y


-- 0.10

data IntArb = ILeaf | IArb Int IntArb IntArb

instance Show IntArb where
    show ILeaf = "()"
    show (IArb x l r) = "(" ++ show x ++ show l ++ show r ++ ")"

-- -- test:
-- ghci> show (IArb 2 (IArb 3 ILeaf ILeaf) (IArb 4 ILeaf ILeaf))
-- "(2(3()())(4()()))"


-- 0.11

data Arb a = Leaf | Node a (Arb a) (Arb a)

arb1 = Node 2 (Node 3 (Node 5 Leaf Leaf) Leaf) (Node 4 Leaf Leaf)

-- explicatie: trb si a sa fie Show pt ca in cadrul show-ului pt Node x l r ne folosim de "show x". trebuie sa putem afisa tipul de baza ca sa afisam nodul
instance (Show a) => Show (Arb a) where
    show Leaf = "()"
    show (Node x l r) = "(" ++ show x ++ show l ++ show r ++ ")"


-- 0.13

instance (Eq a) => Eq (Arb a) where
    (==) Leaf Leaf = True
    (==) (Node x l1 r1) (Node y l2 r2) = x == y && l1 == l2 && r1 == r2
    (==) _ _ = False


-- 0.14

class Pretty a where
    prettyPrint :: a -> String


prettyPrintTreeUtil :: Show a => Arb a -> Int -> String -> String
prettyPrintTreeUtil Leaf indentLevel indentString = "arbore {}"
prettyPrintTreeUtil (Node x l r) indentLevel indentString = let indentation = concat [indentString | r <- [1..indentLevel]] in
    "arbore {\n" ++
    indentation ++ indentString ++ "value: " ++ show x ++ ", \n" ++
    indentation ++ indentString ++ "left: " ++
    prettyPrintTreeUtil l (indentLevel + 1) indentString ++ ", \n" ++
    indentation ++ indentString ++ "right: " ++
    prettyPrintTreeUtil r (indentLevel + 1) indentString ++ "\n" ++
    indentation ++ "}"
-- am scris functia de mai sus inainte sa realizez ca ghci nici nu escape-uieste caractere
-- dar printez un arbore in main


instance Pretty Nat' where
    prettyPrint :: Nat' -> String
    prettyPrint Zero = "~* zero *~"
    prettyPrint (Succ x) = "~ the succesor of " ++ prettyPrint x

instance (Show a) => Pretty (Arb a) where
    prettyPrint arb = prettyPrintTreeUtil arb 0 "\t"


-- 0.15

class MyNum a where
    toInt :: a -> Int

-- care Nat trebe?!?!?!
-- voi presupune ca Nat'-ul meu, asta cu Succ

instance MyNum Nat' where
    toInt Zero = 0
    toInt (Succ x) = 1 + toInt x


-- 0.16

mulAccum :: Nat' -> Nat' -> Nat' -> Nat' -> Nat'
mulAccum x y iteration total | iteration == Zero = total 
mulAccum x y iteration total = mulAccum x y (iteration - Succ Zero) (total + x)

mul :: Nat' -> Nat' -> Nat'
-- mai intai cazuri cat de cat optimizate
mul Zero _ = Zero
mul _ Zero = Zero
mul (Succ Zero) x = x
mul x (Succ Zero) = x
mul x y | x < y = mul y x
mul x y = mulAccum x y y Zero

fromIntegerToNatPrimeAccum :: Integer -> Nat' -> Nat'
fromIntegerToNatPrimeAccum x a | x == 0 = a
fromIntegerToNatPrimeAccum x a = fromIntegerToNatPrimeAccum (x - 1) (Succ a)

fromIntegerToNatPrime :: Integer -> Nat'
fromIntegerToNatPrime x | x <= 0 = Zero
fromIntegerToNatPrime x = fromIntegerToNatPrimeAccum x 0

instance Num Nat' where
    x + Zero = x
    x + Succ y = Succ x + y

    x - y | x <= y = Zero
    x - Zero = x
    Succ x - Succ y = x - y

    (*) = mul

    abs x = x

    signum x | x == Zero = 0
    signum x = 1

    fromInteger = fromIntegerToNatPrime

-- 0.17


data List a = Nil | Cons' a (List a)

instance (Eq a) => Eq (List a) where
    (==) Nil Nil = True
    (==) (Cons' a list1) (Cons' b list2) = list1 == list2 && a == b
    (==) _ _ = False


-- 0.18

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons' a list) = Cons' (f a) (fmap f list)




main :: IO ()
main = do
    putStrLn (prettyPrint arb1)