-- 1
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _       = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _         = True

not' :: Bool -> Bool
not' True = False
not' False = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _       = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _         = False

-- not a or b
-- (functie partiala)
implies :: Bool -> Bool -> Bool
implies a = or' (not' a)

doubleImplies :: Bool -> Bool -> Bool
doubleImplies a b = implies a b && implies b a


-- 2
hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b          = False
hasDivisors n a b | n `mod` a == 0 = True
hasDivisors n a b                  = hasDivisors n (a + 1) b

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
isPrime n          = hasDivisors n 2 (n - 1) == False


-- 3
cmmdc :: Integer -> Integer -> Integer
cmmdc x 0 = x
cmmdc x y = cmmdc y (mod x y)

cmmdc' :: Integer -> Integer -> Integer
cmmdc' x y | x == y = x
cmmdc' x y | x >  y = cmmdc y (x - y)
cmmdc' x y | x <  y = cmmdc x (y - x)

cmmdc'' :: Integer -> Integer -> Integer
cmmdc'' x 0 = x
cmmdc'' x y | even x && even y = 2 * cmmdc'' (x `div` 2) (y `div` 2)
            | even x = cmmdc'' (x `div` 2) y
            | even y = cmmdc'' x (y `div` 2)
            | x <= y = cmmdc'' x (y - x)
            | otherwise = cmmdc'' (x - y) y

-- 4
-- tail-recursive binary gcd
-- adun in a cu cat trebuie inmultit la final rezultatul
cmmdcAux :: Integer  -> Integer  -> Integer  -> Integer 
cmmdcAux 0 y a = y * a
cmmdcAux x 0 a = x * a
cmmdcAux x y a  | even x && even y = cmmdcAux (x `div` 2) (y `div` 2) 2*a
                | even x = cmmdcAux (x `div` 2) y a
                | even y = cmmdcAux x (y `div` 2) a
                | x <= y = cmmdcAux x (y - x) a
                | otherwise = cmmdcAux (x - y) y a

cmmdc''' :: Integer  -> Integer  -> Integer 
cmmdc''' x y = cmmdcAux x y 1

-- 5
fiboaux :: Integer  -> Integer  -> Integer  -> Integer 
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n - 1) (a + b) a

fibo' :: Integer  -> Integer 
fibo' n = fiboaux n 0 1

fiboLog :: Integer -> Integer
fiboLog n | n < 3 = 1
          | otherwise = fiboLog (n - 1) + fiboLog (n - 2)

-- 6
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
-- gcd(a, b, x, y)
extendedEuclid a 0 = (a, 1, 0)
extendedEuclid a b = let (cmmdc, x1, y1) = extendedEuclid b (a `mod` b) in
                     let x = y1 in
                     let y = x1 - y1 * (a `div` b) in
                     (cmmdc, x, y)

-- 7
succ' :: Integer -> Integer
succ' x = x + 1

-- 8
addAux :: Integer -> Integer -> Integer -> Integer
addAux x y a | y == a = x
             | otherwise = addAux (succ x) y (succ a)

add' :: Integer -> Integer -> Integer
add' x y = addAux x y 0

-- a tine minte efectiv rezultatul inmultirii
-- b tine minte de cate ori s-a adunat efecti v
mulAux :: Integer -> Integer -> Integer -> Integer -> Integer
mulAux x y rez no | y == 0 = 0
                  | x == 0 = 0
                  | y == no = rez
                  | otherwise = mulAux x y (add' x rez) (succ no)

mul' :: Integer -> Integer -> Integer
mul' x y = mulAux x y 0 0

powAux :: Integer -> Integer -> Integer -> Integer
powAux x y a | y == 0 = 0
             | y == a = x
             | otherwise = powAux (mul' x x) y (succ a)

pow' :: Integer -> Integer -> Integer
pow' x y = powAux x y 1

-- 9
divAux :: Integer -> Integer -> Integer -> Integer
divAux x y a | x < y = a
             | otherwise = divAux (x - y) y (a + 1)

div' :: Integer -> Integer -> Integer
div' x y = divAux x y 0

mod' :: Integer -> Integer -> Integer
mod' x y = x - mul' y (div' x y)