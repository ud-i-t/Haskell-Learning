module Shape where
import Lib

data Point = Point Int Int deriving Show
data Rect = Rect Int Int Int Int deriving Show

offset (Point x1 y1) (Point x2 y2) = 
    Point (x1 + x2) (y1 + y2)

contains (Rect rx ry rw rh) (Point px py)
    | px < rx = False 
    | px > (rx + rw - 1) = False
    | py < ry = False 
    | py > (ry + rh - 1) = False 
    | otherwise = True