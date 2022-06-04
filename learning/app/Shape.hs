module Shape where
import Lib

data Point = Point Int Int | Point3D Int Int Int deriving Show
data Rect = Rect Int Int Int Int | Rect3D Int Int Int Int Int Int deriving Show

offset (Point x1 y1) (Point x2 y2) = 
    Point (x1 + x2) (y1 + y2)

contains (Rect rx ry rw rh) (Point px py)
    | px < rx = False 
    | px > (rx + rw - 1) = False
    | py < ry = False 
    | py > (ry + rh - 1) = False 
    | otherwise = True

contains (Rect3D rx ry rz rw rh rd) (Point3D px py pz)
    | px < rx = False 
    | px > (rx + rw - 1) = False
    | py < ry = False 
    | py > (ry + rh - 1) = False
    | pz < rz = False 
    | pz > (rz + rd - 1) = False  
    | otherwise = True