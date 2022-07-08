-- lesson13
-- 型クラス
-- GHCiの:t コマンドで型を調べることが可能
simple x = x

-- :infoで型と型クラスの情報を得る
-- :info Num
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y)*2