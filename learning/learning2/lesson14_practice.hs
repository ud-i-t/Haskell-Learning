-- 練習問題
-- Q14-1
data MyNumber = N1 | N2 | N3 | N4 deriving (Enum)

instance Ord MyNumber where
    compare d1 d2 = compare (fromEnum d1) (fromEnum d2) 

instance Eq MyNumber where
    (==) d1 d2 = (==) (fromEnum d1) (fromEnum d2) 

-- Q14-2
-- コンパイルはできるがrollした結果が表示できない
-- Ambiguous type variable ‘a0’ arising from a use of ‘print’
-- と出ているからtoEnumの結果の型が定まらないのだろう……
-- 引数だけではどの型を返したらいいか確かにわからない。
-- 一旦おいて先へ進もう……
class (Eq a, Enum a) => Die a where
    roll :: Int -> a

data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show)
instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)