half :: Int -> Double
half n = (fromIntegral n) / 2

-- quick check 11-1
halve :: Int -> Int
halve a = a `div` 2

-- quick check 11-2
printDouble :: Int -> String
printDouble n = show (n * 2)

-- Haskellの関数は内部的には1つしか引数を受け取らない
-- 複数の引数を持つ関数は部分適用を使って呼び出される
makeAddress number street town = (number, street, town)
makeAddressLambda = (\number ->
                    \street ->
                        \town -> (number, street, town))

-- quick check 11-3
-- makeAddress :: Int -> String -> String -> (Int, String, String)
-- makeAddress :: String -> String -> (Int, String, String)
-- makeAddress :: String -> (Int, String, String)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n 