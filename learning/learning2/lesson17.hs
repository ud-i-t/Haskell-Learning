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

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | White deriving (Show, Eq)

instance Semigroup Color where
    (<>) White b = b
    (<>) a White = a
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

instance Monoid Color where
    mempty = White
    mappend = (<>)

elemtest = (`elem` [Red, Blue, Purple])

-- monoid
-- class Monoid a where
--  mempty :: a (単位元を返す)
--  mappend :: a -> a -> a (加算)
--  mconcat :: [a] -> a

data Events = Events [String]
data Probs = Probs [Double]
data PTable = PTable Events Probs

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2) --文字列で直積を作ってからEventsを作成
    where combiner = (\x y -> mconcat [x,"-",y])

instance Semigroup Events where
    (<>) = combineEvents

instance Monoid Events where
    mempty = (Events [])
    mappend = (<>)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
    (<>) = combineProbs

instance Monoid Probs where
    mempty = (Probs [])
    mappend = (<>)


instance Show PTable where
    show (PTable (Events events) (Probs probs)) = mconcat pairs
        where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
    (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = e1 <> e2
              newProbs = p1 <> p2

instance Monoid PTable where
    mempty = PTable (Events []) (Probs [])
    mappend = (<>)

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = Probs (map (\x -> x/totalProbs) probs) --全ての確率の和が1になるようにする

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

-- testData
coin :: PTable
coin = createPTable (Events ["heads","tails"]) (Probs [0.5,0.5])

spinner :: PTable
spinner = createPTable (Events ["red","blue","green"]) (Probs [0.1,0.2,0.7])

-- 次は練習問題
