-- lesson18
-- パラメータ化された型

import Data.List

-- 任意の型を保持できる抽象コンテナBox
data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box a) = a

-- 任意の同じ型3つを保持できる抽象コンテナTriple
data Triple a = Triple a a a deriving Show

type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

-- Tripleから値を取り出す
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a ) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)