module Main where

import Lib

fact 0 = 1
fact n | n > 0 = n * fact (n - 1)

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = do
    print c
    print $ fact 5
    print $ fib 6
    print $ fib 7
    where
        a = 1
        b = 2
        c = a + b
    