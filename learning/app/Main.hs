module Main where

import Lib
import qualified Fact
import qualified Color
import qualified Product

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


main = do
    print c
    print $ Fact.fact 5
    print $ fib 6
    print $ fib 7
    print $ Color.mix Color.Red Color.Red
    print $ Color.mix Color.Red Color.Green
    print $ Color.mix Color.Red $ Color.mix Color.Green Color.Blue
    print $ p3
    print $ Product.contains (Product.Rect 2 2 3 3) (Product.Point 1 1)
    print $ Product.contains (Product.Rect 2 2 3 3) (Product.Point 2 2)
    print $ Product.contains (Product.Rect 2 2 3 3) (Product.Point 3 3)
    print $ Product.contains (Product.Rect 2 2 3 3) (Product.Point 4 4)
    print $ Product.contains (Product.Rect 2 2 3 3) (Product.Point 5 5)

    where
        a = 1
        b = 2
        c = a + b
        p1 = Product.Point 2 3
        p2 = Product.Point 1 1
        p3 = Product.offset p1 p2
