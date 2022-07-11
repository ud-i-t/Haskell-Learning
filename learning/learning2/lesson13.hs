-- lesson13
-- 型クラス
-- GHCiの:t コマンドで型を調べることが可能
simple x = x

-- :infoで型と型クラスの情報を得る
-- :info Num
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y)*2

class Describable a where
    describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n