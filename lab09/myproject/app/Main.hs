module Main (main) where

import Nat
import NatConversions


inputToNumber :: String -> Int
inputToNumber x = read x :: Int 


main :: IO ()
main = do
    x <- fmap (read :: String -> Int) getLine  -- acum x e un IO Int
    y <- fmap (read :: String -> Int) getLine  -- acum y e un IO Int
    putStrLn (show (x + y))     -- de aia voiam IO ints :)
