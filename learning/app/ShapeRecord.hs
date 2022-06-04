module ShapeRecord where
import Lib

data Point = Point Int Int deriving Show
data Rect = Rect Int Int Int Int deriving Show

contains (Rect rx ry rw rh) (Point px py)
    | px < rx = False 
    | px > (rx + rw - 1) = False
    | py < ry = False 
    | py > (ry + rh - 1) = False 
    | otherwise = True
