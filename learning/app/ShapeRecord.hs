module ShapeRecord where
import Lib

data Point = Point {px :: Int, py :: Int} deriving Show
data Rect = Rect {rx :: Int, ry :: Int, rw :: Int, rh :: Int} deriving Show

contains (Rect rx ry rw rh) (Point px py)
    | px < rx = False 
    | px > (rx + rw - 1) = False
    | py < ry = False 
    | py > (ry + rh - 1) = False 
    | otherwise = True
