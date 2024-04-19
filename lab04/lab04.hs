-- pre-seminar:

data Nat = Int
data Error a = Error String | Value a deriving (Show) 
h:: Error Nat -> (Nat -> Error Nat) -> Error Nat
h z@(Error eroare) functie = z
h (Value v) functie = functie v


-- 1.1

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- 2.1

doubleInt :: Int -> Int
doubleInt x = 2 * x

myFunAccum ::  (Int -> Int) -> Int -> Int -> Int -> Int
myFunAccum f x y a | x > y = a
                   | otherwise = myFunAccum f (x + 1) y (a + f x)

myFun :: (Int -> Int) -> Int -> Int -> Int
myFun f x y = myFunAccum f x y 0

-- 2.2

composedFunction :: (b -> c) -> (a -> b) -> a -> c
composedFunction f g x = f (g x)

-- 2.3
composeListFunctions :: [a -> a] -> (a -> a)
composeListFunctions = foldr composedFunction id    -- cu foldr :)
-- composeListFunctions []         = id
-- composeListFunctions (x : xs) a = x (composeListFunctions xs a)

-- 2.4

sumReduceAux :: Num a => [a] -> a -> a
sumReduceAux [] a       = a
sumReduceAux (x : xs) a = sumReduceAux xs (a + x) 

sumReduce :: Num a => [a] -> a
sumReduce list = sumReduceAux list 0

-- 2.5

myMap :: (a -> a) -> [a] -> [a]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

-- 2.6

myFilterAux :: (a -> Bool) -> [a] -> [a] -> [a]
myFilterAux _ [] list = list                                         
myFilterAux f (x : xs) existingList | f x       = myFilterAux f xs (existingList ++ [x]) -- este vreun fel mai ok de a pune elementul la sfarsit ??  
                                    | otherwise = myFilterAux f xs  existingList 

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []   = []
myFilter f list = myFilterAux f list []

-- 2.7

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f base [] = base
foldr' f base (x : xs) = f x (foldr' f base xs)


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f base [] = base
foldl' f base (x : xs) = foldl' f (f base x) xs

-- 2.8

data Arb a = Frunza | Nod a (Arb a) (Arb a) deriving (Show)

arb1 :: Arb Integer
arb1 = Nod 5 (Nod 4 Frunza Frunza) (Nod 7 (Nod 6 Frunza Frunza) Frunza)

-- functii stil "visitor"
preOrder :: Ord a => (a -> a) -> Arb a -> [a]
preOrder f Frunza = []
preOrder f (Nod x arb1 arb2) = f x : preOrder f arb1 ++ preOrder f arb2

inOrder :: Ord a => (a -> a) -> Arb a -> [a]
inOrder f Frunza = []
inOrder f (Nod x arb1 arb2) = inOrder f arb1 ++ [f x] ++ inOrder f arb2

postOrder :: Ord a => (a -> a) -> Arb a -> [a]
postOrder f Frunza = []
postOrder f (Nod x arb1 arb2) = postOrder f arb1 ++ postOrder f arb2 ++ [f x]

-- 2.9
parcurgere :: Ord a => (a -> a) -> ((a -> a) -> Arb a -> [a]) -> Arb a -> [a]
parcurgere f parc = parc f

-- 3.1
-- presupunem ca functia compare a b returneaza a<b=False

fun :: Ord a => a -> a -> Bool
fun a b = a >= b

-- quickSort
sortByCompare :: [a] -> (a -> a -> Bool) -> [a]
sortByCompare (x : xs) f =  filter (not . f x) xs ++ [x] ++ filter (f x) xs

-- 3.2 -- nu inteleg ce trebuie efectiv sa implementez totusi
data Either' a b = Left a | Right b deriving (Show)
