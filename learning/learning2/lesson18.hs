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

-- 独自にList型を定義
-- 再帰になっている
data List a = Empty | Cons a (List a) deriving Show

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

-- タプルのリスト
itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils", 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

--型が違うので追加できない
itemCount4 :: (String, Double)
itemCount4 = ("Paper", 12.4)

itemInventry :: [(String, Int)]
itemInventry = [itemCount1, itemCount2, itemCount3]