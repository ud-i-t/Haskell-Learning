module Product where
import Lib

data Point = Point Int Int deriving Show

offset (Point x1 y1) (Point x2 y2) = 
    Point (x1 + x2) (y1 + y2)