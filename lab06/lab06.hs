minim :: (Ord a) => ([a] -> [a]) -> [a] -> Maybe a
minim sortFunction []   = Nothing
minim sortFunction list = Just $ head $ sortFunction list

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (hd : tl) = quickSort(filter (<= hd) tl) ++ [hd] ++ quickSort (filter (> hd) tl)

-- ghci> minim quickSort $ genList 5000
-- Just 1
-- (2.06 secs, 1,114,123,200 bytes)

-- ghci> quickSort $ genList 5000
-- (5.51 secs, 2,172,082,240 bytes)


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (hd : tl) | x > hd = hd : insert x tl
                   | otherwise = x : hd : tl

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (hd : tl) = insert hd (insertionSort tl)

-- ghci> minim insertionSort $ genList 5000  
-- Just 1
-- (0.01 secs, 2,340,824 bytes)

-- ghci> insertionSort $ genList 5000
-- (4.88 secs, 3,175,189,312 bytes)

-- ghci> last $ insertionSort $ genList 5000 
-- 5000
-- (3.58 secs, 3,152,586,496 bytes)

deleteFromList :: Eq a => [a] -> a -> [a]
deleteFromList [] a = []
deleteFromList (hd : tl) a | hd == a = tl
                           | otherwise = hd : deleteFromList tl a

selectionSort :: (Ord a, Eq a) => [a] -> [a]
selectionSort [] = []
selectionSort list = let x = minimum list in x : selectionSort (deleteFromList list x)

-- ghci> selectionSort $ genList 5000
-- (7.84 secs, 2,524,710,440 bytes)
-- ghci> minim selectionSort ( genList 5000)  
-- Just 1
-- (0.00 secs, 460,664 bytes)
-- ghci> last $ selectionSort ( genList 5000) 
-- 5000
-- (6.50 secs, 2,501,878,160 bytes)

maxim ::  (Ord a, Eq a) => ([a] -> [a]) -> [a] -> Maybe a
maxim sortFunction []        = Nothing
maxim sortFunction [hd]      = Just hd
maxim sortFunction (hd : tl) = maxim sortFunction tl

bubbleSortPassThrough :: Ord a => [a] -> [a]
bubbleSortPassThrough [] = []
bubbleSortPassThrough [hd] = [hd]
bubbleSortPassThrough (hd : hd2 : tl) = if hd <= hd2 then
                                            hd : bubbleSortPassThrough (hd2 : tl)
                                        else
                                            hd2 : bubbleSortPassThrough (hd : tl)

bubbleSortAux :: Ord a => [a] -> Int -> [a]
bubbleSortAux list x | x == length list = list
                     | otherwise = bubbleSortAux (bubbleSortPassThrough list) (x + 1)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort list = bubbleSortAux list 0

-- banuiesc ca fiindca in bubbleSort am defapt un loop fortat de complexitate O(n) (apelez bubbleSortPassThrough de n ori), nu vor fi diferente de timp
-- daca gresesc, probabil aflu eu pana data viitoare :)

-- ghci> length $ bubbleSort [1, 2 .. 1000]
-- 1000
-- (0.44 secs, 336,146,032 bytes)
-- ghci> length $ bubbleSort $ genList 1000
-- 1000
-- (0.48 secs, 336,146,248 bytes)

-- ghci> minim bubbleSort [1, 2 .. 1000]   
-- Just 1
-- (0.49 secs, 336,148,480 bytes)
-- ghci> minim bubbleSort $ genList 1000   
-- Just 1
-- (0.49 secs, 336,148,624 bytes)

-- ghci> last $ bubbleSort [1, 2 .. 1000]
-- 1000
-- (0.46 secs, 336,146,176 bytes)
-- ghci> last $ bubbleSort $ genList 1000
-- 1000
-- (0.49 secs, 336,146,320 bytes)


-- 5

fibAux :: Int -> Int -> [Int]
fibAux i j = i : fibAux j (i + j)

fib :: [Int]
fib = fibAux 1 1

isPrimeAux :: Int -> Int -> Bool
isPrimeAux i j | j > div i 2 = True
               | mod i j == 0 = False
               | otherwise = isPrimeAux i (j + 1) 

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = isPrimeAux x 2

listAux :: Int -> [Int]
listAux x = x : listAux (x + 1)

listNat :: [Int]
listNat = listAux 0

listBoolPrime = map isPrime listNat

listPrime = filter (listBoolPrime !!) listNat

genList n = [n, n - 1 .. 1]