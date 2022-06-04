module Main where

import Lib
import qualified Fact
import qualified Color
import qualified Shape

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
    print $ Shape.contains (Shape.Rect 2 2 3 3) (Shape.Point 1 1)
    print $ Shape.contains (Shape.Rect 2 2 3 3) (Shape.Point 2 2)
    print $ Shape.contains (Shape.Rect 2 2 3 3) (Shape.Point 3 3)
    print $ Shape.contains (Shape.Rect 2 2 3 3) (Shape.Point 4 4)
    print $ Shape.contains (Shape.Rect 2 2 3 3) (Shape.Point 5 5)

    where
        a = 1
        b = 2
        c = a + b
        p1 = Shape.Point 2 3
        p2 = Shape.Point 1 1
        p3 = Shape.offset p1 p2
