-- lesson20
-- Maybe型

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int, Double)]
file1 = [(1, 200.1), (2, 199.5), (3, 199.4), (4, 198.9), (5, 199.0), (6, 200.2),
         (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int, Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5), (15, 204.9), (16, 207.1),
         (18, 210.5), (20, 208.8)]

file3 :: [(Int, Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5), (17, 210.5),
         (24, 215.1), (25, 218.7)]

file4 :: [(Int, Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8), (29, 222.8), (30, 223.8), (31, 221.7),
         (32, 222.3), (33, 220.8), (34, 219.4), (35, 220.1), (36, 220.6)]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extenedValues
    where completeTimes = [minimum times .. maximum times]
          timeValueMap = Map.fromList (zip times values)
          extenedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
    where splitPairs = (unzip tvPairs)
          times = fst splitPairs
          values = snd splitPairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time,"|",show value,"\n"]
showTVPair time Nothing = mconcat [show time,"|NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
        where rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- MapはMaybeを受け付けないので、値が有効の場合のみinsertするヘルパー関数を作成
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_,Nothing) = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where bothTimes = mconcat [t1, t2]
          completeTimes = [(minimum t1) .. (maximum t2)]
          tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
          updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
          combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

-- TSをSemigroupのインスタンスにする
instance Semigroup (TS a) where
    (<>) = combineTS

-- TSをMonoidのインスタンスにする
instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

-- 4つの時系列データを結合
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs = total/count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
    where justVals = filter isJust values
          cleanVals = map (\(Just x) -> x) justVals
          avg = mean cleanVals

-- 型シノニム
type CompareFunc a = (a -> a -> a)
type TSCompareFunc a = ((Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a))

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
    where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing) -- 両方Nothingの場合
          newFunc (i, val) (_, Nothing) = (i, val)            -- 片方がNothingの場合
          newFunc (_, Nothing) (i, val) = (i, val)            -- 片方がNothingの場合
          newFunc (i1, Just val1) (i2, Just val2) = if (func val1 val2) == val1
                                                    then (i1, Just val1)
                                                    else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing
                                   else Just best
    where pairs = zip times values
          best = foldl (makeTSCompare func) (0, Nothing) pairs -- zipってどんな型を返すんだっけ…

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max
