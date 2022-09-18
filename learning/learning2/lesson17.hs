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

-- monoid
-- class Monoid a where
--  mempty :: a (単位元を返す)
--  mappend :: a -> a -> a (加算)
--  mconcat :: [a] -> a

type Events = [String]
type Probs = [Double]
data PTable = PTable Events Probs

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x/totalProbs) probs --全ての確率の和が1になるようにする

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          -- . は関数合成, repeatはl1を要素として無限リストを返す
          repeatdL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatdL1
          -- cycle は単一の無限リストを返す
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

-- testData
coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- 次は練習問題