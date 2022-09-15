-- lesson17
-- 合成によるデザイン

import Data.List
import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

-- リストのすべての要素である特性がTrueかどうかをテスト
myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

-- Quick check 17-1
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

-- 17.2 Semigroup
instance Semigroup Integer where
    (<>) x y = x + y -- <> 演算子を加算として定義

-- (<>) の型シグネチャ
-- (<>) :: Semigroup a => a -> a -> a

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Red Yellow = Orange
    (<>) Yellow Red = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a,b] = Purple      -- 部分適用したelemをallの第1引数として渡す
             | all (`elem` [Yellow, Red, Orange]) [a,b] = Orange
             | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
             | otherwise = Brown

-- 結合律: <>演算子を適用する順序によって結果が変わらないこと


elemtest = (`elem` [Red, Blue, Purple])