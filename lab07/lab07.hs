import Data.Char
import System.IO
import System.Environment

-- 0.2

main02 :: IO ()
main02 = putStrLn "Hi!" >> putStrLn "another line" >> putStrLn "da."

-- 0.3

main03 :: IO ()
main03 = putStrLn "Prenume:" >> getLine >>= 
    (\prenume -> putStrLn "Nume: " >> getLine >>= (\nume -> putStrLn ("Buna, " ++ nume ++ " " ++ prenume ++ "!")))

-- 0.4

main04 :: IO ()
main04 = do
         putStrLn "Prenume:"
         prenume <- getLine
         putStrLn "Nume:"
         nume <- getLine
         putStrLn ("Buna, " ++ nume ++ " " ++ prenume ++ "!")

-- 0.5

main05 :: IO ()
main05 = putStrLn "What is your name?" >> getLine >>= (\name -> putStrLn ("Hello, " ++ name ++ "!")) >> main05

-- 0.6

main06 :: IO ()
main06 = putStrLn "Prenume:" >> getLine >>= 
    (\prenume -> putStrLn "Nume: " >> getLine >>= (\nume -> putStrLn ("Buna, " ++ nume ++ " " ++ prenume ++ "!")  >> main06))

-- 0.7

main07 :: IO ()
main07 = putStrLn "Prenume:" >> getLine >>= 
    (\prenume -> putStrLn "Nume: " >> getLine >>= (\nume -> putStrLn ("Buna, " ++ nume ++ " " ++ prenume ++ "!") >> (if prenume == "" || nume == "" then (return ()) else main07))) 

-- 0.8

stringToUpper :: String -> String
stringToUpper = map toUpper

main08 :: IO ()
main08 = do
    putStrLn "Enter string to capitalize:"
    sir <- getLine
    if sir /= "" then
        putStrLn (stringToUpper sir) >> main08
    else putStrLn "Exiting..."

-- 0.9 

-- ghci> :i openFile    
-- openFile :: FilePath -> IOMode -> IO Handle
--         -- Defined in `GHC.IO.StdHandles'
-- ghci> :i hGetContents
-- hGetContents :: Handle -> IO String
--         -- Defined in `GHC.IO.Handle.Text'
-- ghci> :i hGetLine    
-- hGetLine :: Handle -> IO String         -- Defined in `GHC.IO.Handle.Text'
-- ghci> :i hClose 
-- hClose :: Handle -> IO ()       -- Defined in `GHC.IO.Handle'
-- ghci> :i getArgs
-- getArgs :: IO [String]  -- Defined in `System.Environment'
-- ghci> :i getProgName
-- getProgName :: IO String        -- Defined in `System.Environment'
-- ghci> :i hPutStr
-- hPutStr :: Handle -> String -> IO ()
--         -- Defined in `GHC.IO.Handle.Text'

-- 0.10

main10 :: IO ()
main10 = do
    handle <- openFile "exemplu.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    
-- 0.11 + 0.12

main11 :: IO ()
main11 = do
    name <- getProgName
    args <- getArgs
    if length args /= 1 then putStrLn ("Usage: " ++ name ++ " <filename>")
    else 
        do
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        putStrLn contents

-- main :: IO ()
-- main = main11        

-- PS C:\Users\asus\Documents\Facultate\PF\Labs> ghc -o lab07_11 lab07.hs
-- [1 of 1] Compiling Main             ( lab07.hs, lab07.o )
-- Linking lab07_11.exe ...
-- PS C:\Users\asus\Documents\Facultate\PF\Labs> .\lab07_11.exe
-- Usage: lab07_11.exe <filename>
-- PS C:\Users\asus\Documents\Facultate\PF\Labs> .\lab07_11.exe exemplu.txt
-- text
-- asdf
-- PS C:\Users\asus\Documents\Facultate\PF\Labs> 

-- 0.13

main13 :: IO ()
main13 = do
    name <- getProgName
    args <- getArgs
    if length args /= 1 then putStrLn ("Usage: " ++ name ++ " <filename>")
    else 
        do
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        putStrLn (stringToUpper contents)

-- 0.14

convertToBool :: String -> Maybe Bool
convertToBool msg = let lowMsg = map toLower msg in
                    if lowMsg == "da" then Just True
                    else if lowMsg == "nu" then Just False
                    else Nothing

midInterval :: Int -> Int -> Int
midInterval x y = (x + y + 1) `div` 2

main14 :: Int -> Int -> IO ()
main14 x y = let mid = midInterval x y in
    do
    if x == y then putStrLn $ "Numarul este " ++ show x
    else do
        putStrLn $ "Numere " ++ show x ++ " " ++ show y
        putStrLn $ "Numarul de ghicit este >=" ++ show mid ++ "?"
        response <- getLine
        let evalResp = convertToBool response in
            case evalResp of
                Nothing -> do
                    putStr "Nu am inteles. "
                    main14 x y
                Just True -> 
                    main14 mid y
                Just False -> main14 x (mid - 1)

main14_propriuzis :: IO ()
main14_propriuzis = main14 1 100
    

-- 0.16

main16 :: IO ()
main16 = do
    args <- getArgs
    handle <- openFile "loremipsum.txt" ReadMode
    contents <- hGetContents handle
    (if length args == 1 then do
        print $ length contents
    else putStrLn "this should take way less time")
    hClose handle

main = main16


-- unrelated, voiam sa testez ceva legat de lazy evaluation
expensiveMapFxnAux :: Integer -> a -> a
expensiveMapFxnAux aux el | aux >= 10000000000000 = el
                          | otherwise = expensiveMapFxnAux (aux * 2) el

expensiveMap :: [a] -> [a]
expensiveMap = map (expensiveMapFxnAux 0)


-- 0.17: 

inputToNumber :: String -> Int
inputToNumber x = read x :: Int 

-- similar cu o functie de hashing defapt. dar mi s-a cerut pseudorandom deci...
randomIntWithSeed :: Int -> Int
randomIntWithSeed x =  (x * 2654435761) `mod` (2^32)

randomIntList :: Int -> [Int]
randomIntList x | x <= 0 = []
randomIntList x = map randomIntWithSeed [1, 2..x]

main17 :: IO ()
main17 = do
    number <- getLine
    print (randomIntList (inputToNumber number))