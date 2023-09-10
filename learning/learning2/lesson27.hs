-- lesson27

-- 27.1
successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

-- この関数はMaybeのコンテキスト専用 他のコンテキスト用のincは別途書かないといけない…。
incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

-- 一般化 (すでにGHC.Baseに標準の実装がある)
-- instance Functor Maybe where
--   fmap func (Just n) = Just (func n)
--   fmap func Nothing = Nothing

