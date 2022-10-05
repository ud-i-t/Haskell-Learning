-- lesson18
-- パラメータ化された型

import Data.List
import qualified Data.Map as Map

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

--Data.Map
data Organ = Heart | Brain | Kidney | Spleen deriving (Show,Eq,Ord,Enum)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

--練習問題
--Q18-1
tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func (Triple x y z) = Triple (func x) (func y) (func z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap func (Box x) = Box (func x)

--Q18-2
--Organをキーにできるようにする
values :: [Organ]
values = map snd (Map.toList organCatalog) 

-- 全臓器の種類のリスト
allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

-- 各種臓器の個数を数える
organCounts :: [Int]
organCounts = map countOrgan allOrgans
    where countOrgan = (\organ -> (length . filter (== organ)) values)

organInventry :: Map.Map Organ Int
organInventry = Map.fromList (zip allOrgans organCounts)
