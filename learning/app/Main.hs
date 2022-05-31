module Main where

import Lib
import qualified Fact

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data Color = Blue | Red | Green | White deriving (Show, Enum)

main = do
    print c
    print $ Fact.fact 5
    print $ fib 6
    print $ fib 7
    print Blue
    print $ fromEnum Red
    print (toEnum 1 :: Color)
    print (toEnum 2 :: Color)

    where
        a = 1
        b = 2
        c = a + b
    