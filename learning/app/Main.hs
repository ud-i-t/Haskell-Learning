module Main where

import Lib
import qualified Fact
import qualified Color
import ShapeRecord
import Action

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data Foo = TestInt Int 
        | TestStr String
        deriving Show
        
foo (TestInt 1) = "bar"
foo (TestStr "1") = "baz"
foo _ = "?"

main = do
    print c
    print $ Fact.fact 5
    print $ fib 6
    print $ fib 7
    
    print $ Color.mix Color.Red Color.Red
    print $ Color.mix Color.Red Color.Green
    print $ Color.mix Color.Red $ Color.mix Color.Green Color.Blue
    -- print $ p3

    print $ contains (Rect { rx = 2, ry = 2, rw = 3, rh = 3}) (Point { px = 1, py = 1})
    print $ contains (Rect 2 2 3 3) (Point 2 2)
    print $ contains (Rect 2 2 3 3) (Point 3 3)
    print $ contains (Rect 2 2 3 3) (Point 4 4)
    print $ contains (Rect 2 2 3 3) (Point 5 5)
    print (Point 4 5)
    -- print $ contains (Rect3D 2 2 2 3 3 3) (Point3D 1 1 1)
    -- print $ contains (Rect3D 2 2 2 3 3 3) (Point3D 2 2 2)
    -- print $ contains (Rect3D 2 2 2 3 3 3) (Point3D 3 3 3)
    -- print $ contains (Rect3D 2 2 2 3 3 3) (Point3D 4 4 4)
    -- print $ contains (Rect3D 2 2 2 3 3 3) (Point3D 5 5 5)

    print $ foo $ TestInt 1
    print $ foo $ TestStr "1"
    print $ foo $ TestStr "2"
    -- print $ foo "0" 関数の引数は同一の型しか受け付けないのでこれは無理

    r <- randNum
    print r
    print =<< randNum
    randNum >>= print
    
    where
        a = 1
        b = 2
        c = a + b
        -- p1 = Shape.Point 2 3
        -- p2 = Shape.Point 1 1
        -- p3 = Shape.offset p1 p2
